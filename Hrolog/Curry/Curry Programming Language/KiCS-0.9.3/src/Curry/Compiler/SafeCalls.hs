{-# OPTIONS -cpp  #-} 
{-# LANGUAGE FlexibleInstances  #-} 

module Curry.Compiler.SafeCalls where

#if __GLASGOW_HASKELL__ >= 610
import Control.OldException 
#else
import Control.Exception 
#endif

import System.Exit
import System.Process

import Prelude hiding (catch)

--------------------
-- safe calls
--------------------

data Safe m a = Safe (m (Maybe a))

(>>+) :: (Monad m) => m (Maybe a) -> m (Maybe b) -> m (Maybe b)
m >>+ f = m >>=+ (\_ -> f)

(>>=+) :: (Monad m) => m (Maybe a) ->  (a -> m (Maybe b)) -> m (Maybe b)
m >>=+ f = do 
  res <- m
  maybe (return Nothing) f res

instance Monad (Safe IO) where
  return x = Safe (return (Just x))
  (Safe act) >>= f = Safe $ do 
     res <- act
     maybe (return Nothing) (\x->let Safe act = f x in act) res
  fail s = Safe (putStrLn s >> return Nothing)

safeSystem :: Bool -> String -> Safe IO ()
safeSystem _ "" = Safe $ return (Just ())
safeSystem verbose sysCall = Safe $ do
  if verbose then putStrLn sysCall else return ()
  ec <- system sysCall
  if ec==ExitSuccess then return (Just ()) else return Nothing

safeIO :: IO a -> Safe IO a
safeIO action = Safe $ do
  catch (action >>= return . Just)
        putErr

safeIOSeq :: IO a -> Safe IO a
safeIOSeq action = Safe $ do
  catch (action >>= \x -> seq x (return (Just x)))
        putErr 

safe :: Safe m a -> m (Maybe a)
safe (Safe act) = act

putErr e = putStrLn ("IO action failed: "++show e) >> return Nothing
