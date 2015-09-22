--- Provides the interface that is used to generate and send events.
---
--- @version Feb, 2007
---
module Oracle (
  Ref, 

  partCons, partFunc, partCall,
  apply, ($!) ,($!!), ($#), ($##), (>>=),
  compose, unknown,

  oracle, oracleIO,

  fresh, replace, collapse, closeRef, expand

  )where

import IOExts (getAssoc)
import CEventOracle
import qualified Meta
import qualified Unsafe (unsafePerformIO)
--import Global

--extFileName :: Global String
--extFileName = global "" Temporary

infixr 0 $!, $!!, $#, $##

--- Wrapper for constructor with one missing argument.
--  We have to close the reference here, since
--  a value is constructed
partCons :: (a -> b) -> Ref -> a -> b
partCons c r x = closeRef r (c x)
-- partCons c r = collapse r c

--- Wrapper for function with one missing argument.
--  We should not count a step here, since
--  the wrapper is not part of the original program.
partFunc :: (a -> Ref -> b) -> Ref -> a -> b
partFunc f r x = f x r

--- Wrapper for call with more than one missing argument.
partCall :: a -> Ref -> a
partCall x r = closeRef r x

--- Wrapper for apply function.
apply :: (Ref -> a -> b) -> a -> Ref -> b
apply f x r = CEventOracle.apply f x r

--- Wrapper for hnf apply function.
($!) :: (Ref -> a -> b) -> a -> Ref -> b
(f $! x) r = (f CEventOracle.$! x) r 

--- Wrapper for nf apply function.
($!!) :: (Ref -> a -> b) -> a -> Ref -> b
($!!) f x r = (f CEventOracle.$!! x) r 

--- Wrapper for ghnf apply function.
($#) :: (Ref -> a -> b) -> a -> Ref -> b
($#) f x r = (f CEventOracle.$# x) r 

--- Wrapper for gnf apply function.
($##) :: (Ref -> a -> b) -> a -> Ref -> b
($##) f x r = (f CEventOracle.$## x) r 

headNormalFormIO :: (Ref -> a -> Ref -> () -> IO b) -> a -> Ref -> Ref -> () -> IO b
headNormalFormIO f x r = error "headNormalFormIO"

prim_unsafePerformIO :: (Ref -> () -> IO a) -> Ref -> a
prim_unsafePerformIO act ref = replace ref (Unsafe.unsafePerformIO (act ref ()))


lambda_world :: IO a -> Ref -> () -> IO a
lambda_world act ref _ = collapse ref act

--- Wrapper for bind in io monad
(>>=) :: (Ref -> () -> IO a) -> (Ref -> a -> Ref -> () -> IO b) -> Ref -> Ref -> () -> IO b
(>>=) act cont ref = collapse ref (partFunc (bind act cont))

bind :: (Ref -> () -> IO a) -> (Ref -> a -> Ref -> () -> IO b) -> () -> Ref -> IO b
bind act cont world ref = 
  let ref2 = fresh () 
   in expand ref [ref2] (apply act world ref Prelude.>>= bind2 cont world ref2)

bind2 :: (Ref -> a -> Ref -> () -> IO b) -> () -> Ref -> a -> IO b 
bind2 cont world ref2 x =
      let ref3 = fresh () 
       in expand ref2 [ref3] (apply (apply cont x ref2) world ref3)

-- catchFail is also an external higher order function
catchFail :: (Ref -> () -> IO a) -> (Ref -> () -> IO a) -> Ref -> Ref -> () -> IO a
catchFail act1 act2 ref = 
  collapse ref (partFunc (catch act1 act2))

catch :: (Ref -> () -> IO a) -> (Ref -> () -> IO a) -> () -> Ref -> IO a
catch act1 act2 world ref = let ref' = fresh () in
  expand ref [ref'] 
    (Prelude.catchFail 
      (apply act1 world ref  Prelude.>>= return . (,) True)
      (apply act2 world ref' Prelude.>>= return . (,) False) Prelude.>>=
      \ (b,res) -> safeIOResult (return b) >> return res)
    

--- Function composition (Prelude version may be transformed)
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

--- Computes the oracle for a computation. 
oracle :: (Ref -> a) -> IO ()
oracle expr = initialize (return . expr)

oracleIO :: (Ref -> Ref -> () -> IO a) -> IO ()
oracleIO app = initialize (\ mainR -> 
  let appRef = fresh () 
   in expand mainR [appRef] (app mainR appRef ()))


safeIOResult :: IO a -> IO a
safeIOResult act = do 
  Just fn <- getAssoc "extfn"
  x <- act
  let sx = show x
  appendFile fn (show (length sx)++'\n':sx)
  return x


unknown :: Ref -> a
unknown r = CEventOracle.unknown r