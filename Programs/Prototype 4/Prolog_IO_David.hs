import STVarExperiment

import Data.Traversable
import Control.Monad
import Data.Functor
import Control.Applicative
import System.IO

data PrologResult
   = NoResult
   | Cons OneBinding PrologResult 
   | IOIn (IO String) (String -> PrologResult)
   | IOOut (IO ()) PrologResult



data OneBinding = Pair VariableName VariableName


--data MiniLang a = MyData a | Empty | Input   

--runInIO :: PrologResult -> IO [OneBinding]


data PrologIO a = Input (IO a) | Output (a -> IO ()) | PrologData a | Empty 
--				deriving (Show, Eq, Ord)
{--
instance Functor (PrologIO) where
	fmap f Empty 					= Empty
	fmap f (Input (IO a)) 			= Input (IO (f a))
--	fmap f (Output (a -> IO ()))	= Output (a -> IO ())
--	fmap f (PrologData a)			= PrologData (f a)
--}

instance Monad PrologIO where
	 	return a = PrologData a
--	 	(Input i) >>= (Output o) = i >>= (\a -> (o a))

instance (Show a) => Show (PrologIO a) where
	show (Empty) 		= show "No result"
	show (PrologData a) = show a
--	show (Input f)		= show (f ++ "")
--	show (Output )		


-- (>>=) Action sequencer and combiner :- read -> write -> read -> write -> ........
seqio :: PrologIO a -> (a -> PrologIO b) -> PrologIO b
--      (First action   (Take and perform                       
--      which generates  next action)
--      value a) 
seqio (PrologData a) 	f 	= f a
--seqio (Output o) 		f 	= \a -> o a
--seqio (Input i)    		f 	= \s -> (seqio (i s) f) --				Get (\s -> seqio (g s) f)



{--
instance Applicative PrologIO where
	func =

instance Traversable PrologIO where
	traverse f Empty 					= Empty
	traverse f (Input (IO a)) 			= Input (IO (f a))
	traverse f (Output (a -> IO ()))	= Output ((a) -> IO ())
	traverse f (PrologData a)			= PrologData (f a)
--}


concate :: PrologIO t -> PrologIO t -> IO ()
concate (Input f1) (Output f2) = do
	x <- f1
	f2 x
{--
concate (Input getLine) (Output putStrLn)
Loading package list-extras-0.4.1.4 ... linking ... done.
Loading package syb-0.5.1 ... linking ... done.
Loading package array-0.5.0.0 ... linking ... done.
Loading package deepseq-1.3.0.2 ... linking ... done.
Loading package containers-0.5.5.1 ... linking ... done.
Loading package transformers-0.4.3.0 ... linking ... done.
Loading package mtl-2.2.1 ... linking ... done.
Loading package logict-0.6.0.2 ... linking ... done.
Loading package unification-fd-0.10.0.1 ... linking ... done.
1
1
--}