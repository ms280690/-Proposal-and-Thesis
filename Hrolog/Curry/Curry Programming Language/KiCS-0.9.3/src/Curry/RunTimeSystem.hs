{-# OPTIONS -O0 #-} 

module Curry.RunTimeSystem (
  module Curry.RunTimeSystem.BaseCurry,
  module Curry.RunTimeSystem
  ) where

import System.IO
import Curry.RunTimeSystem.BaseCurry
import System.IO.Unsafe
import Data.IORef

-------------------------------------------------
-- normal forms which are all based on ctcStore
-- and may be called from compiled programs.
-------------------------------------------------


--SHOCKING: there was an additional ctcStore False (nf ...) around here, 
-- runtimes were desastrous. Why was that??????
nfCTC :: (BaseCurry a,BaseCurry b) => (b -> Result a) -> b -> Result a
nfCTC cont = ctcStore False (nf cont)

hnfCTC :: (BaseCurry a,BaseCurry b) => (b -> Result a) -> b -> Result a 
hnfCTC = ctcStore False 

gnfCTC :: (BaseCurry a,BaseCurry b) => (b -> Result a) -> b -> Result a
gnfCTC cont = ctcStore True (gnf cont)

ghnfCTC :: (BaseCurry a,BaseCurry b) => (b -> Result a) -> b -> Result a 
ghnfCTC = ctcStore True

-----------------------------------------------------------------
-- treatment for the basic cases of flexible pattern matching
-----------------------------------------------------------------

-- called by generated functions for matching failure
patternFail :: (BaseCurry a,BaseCurry b) => String -> a -> b
patternFail s x = case consKind x of
  Failed -> addException (curryError s) x
  _      -> failed (PatternMatchFail s)


----------------------------------------------------------------------
-- generate logic objects
----------------------------------------------------------------------

-- generate branching
withRef :: (Int -> a) -> Int -> a 
withRef f 0 = f 0
withRef f i = f $! nextRef i

---------------------------------------------------------------
-- manipulating references: the unsafe part of CurryToHaskell
---------------------------------------------------------------

-- the global state of references
storeRefCounter :: IORef Int
{-# NOINLINE storeRefCounter #-}
storeRefCounter = unsafePerformIO (newIORef 1)

-- generate a new reference
nextRef :: Int -> Int 
{-# NOINLINE nextRef #-}
nextRef i = unsafePerformIO (do 
               v <- readIORef storeRefCounter
               writeIORef storeRefCounter (v+i+1)
               return v)

---------------------------------------------------------------
-- run-time options (also unsafe)
---------------------------------------------------------------

-- the easiest way to have different modes for run-time behaviour is
-- a global state of run-time options.
-- the settings are only read once and stay the same during the whole computation.

data RunTimeOptions = RTO {currentModule :: String}

runTimeDefaults :: RunTimeOptions
runTimeDefaults = RTO {currentModule = ""} 

runTimeOptions :: IORef RunTimeOptions
{-# NOINLINE runTimeOptions #-}
runTimeOptions = unsafePerformIO (newIORef runTimeDefaults)

setRunTimeOptions :: RunTimeOptions -> IO ()
setRunTimeOptions = writeIORef runTimeOptions 

freeF :: (BaseCurry b, BaseCurry a) => (b -> a) -> a
freeF = freeOrBased

orF :: BaseCurry a => a -> a -> a
orF = orCTC

-----------------------------------------------------------------------
-- implementation of getProgName (module System) expressions
-----------------------------------------------------------------------

setProgName :: String -> IO ()
setProgName n = do 
  opts <- readIORef runTimeOptions
  writeIORef runTimeOptions (opts{currentModule=n})

setProgNameAndOrBased :: String -> IO ()
setProgNameAndOrBased = setProgName

getProgName :: IO String
getProgName = readIORef runTimeOptions >>= return . currentModule

----------------------------------------------------------------------
-- alternatives for implementation of options
----------------------------------------------------------------------

orCTC :: BaseCurry a => a -> a -> a
orCTC x y = branching (mkRefWithGenInfo NoGenerator (nextRef 0)) [x,y]

-- free variables in or-based mode
freeOrBased :: (BaseCurry b, BaseCurry a) => (b -> a) -> a
freeOrBased f = f (generator (nextRef 0))

----------------------------------------------------------
-- some declarations for external read and show instances
----------------------------------------------------------

ten,eleven,zero :: Int
ten    = 10
eleven = 11
zero   = 0

readQualified :: String -> String -> String -> [((),String)]
readQualified mod name r =  [((),s)  | (name',s)  <- lex r, name' == name] 
                         ++ [((),s3) | (mod',s1)  <- lex r
                                     , mod' == mod
                                     , (".",s2)   <- lex s1
                                     , (name',s3) <- lex s2
                                     , name' == name]



