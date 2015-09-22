{- |
    An experiment to determine whether it is possible to use callCC
    together with getLine & co to capture the semantics of a program
    that has awkward squad entities in it.
-}

import Control.Monad(liftM)
import Control.Monad.Trans.Cont

newtype OneAnswer = OneAnswer { getString :: String }

instance Show OneAnswer where
  showsPrec n = showsPrec n . getString

-- | a result that contains both pure results and I/O based calcuations
data ImpureResult
  = NoMoreAnswers
  | Cons OneAnswer ImpureResult
  | IOIn (IO String) (String -> Cont ImpureResult ImpureResult)
  | IOOut (IO ())  (Cont ImpureResult ImpureResult)

infixr 4 `Cons`     -- syntactic sugar

-- | "purify" a result by running it in the IO monad
runIO :: ImpureResult -> IO [OneAnswer]
runIO thing = do
  case thing of
    NoMoreAnswers  -> return []
    Cons x xs      -> liftM (x:) (runIO xs)
    IOIn g k       -> do { str <- g ; runIO (k str `runCont` id) }
    IOOut xx more  -> xx >> runIO ( more `runCont` id)

-- | prompt the user for input and return it
prompt :: String -> IO String
prompt aPrompt = do
  putStr aPrompt
  getLine

-- | build an impure IOIn result in the continuation monad.
-- require access to the top-level continuation.
prologRead :: (ImpureResult -> Cont ImpureResult String)  -- top-level cont
              -> String -- prompt
              -> Cont ImpureResult String
prologRead topKont aPrompt = callCC ( topKont . IOIn (prompt aPrompt))

-- | build an impure IOOut result in the continuation monad.
-- See below for use.
prologWrite :: String
            -> Cont ImpureResult ImpureResult
            -> Cont ImpureResult ImpureResult
prologWrite output block = return $! IOOut (putStrLn output) block

-- | the test driver.  Change 'testProg3' to anything of TestProgN type.
main :: IO ()
main = do
  let xx = callCC testProg3 `runCont` id
  yy <- runIO xx
  print yy

type TestProgN = ( ImpureResult -> Cont ImpureResult String)
                 -> Cont ImpureResult ImpureResult

-- various test "programs"

testProg1 :: TestProgN
testProg1 _ = do
  liftM  (Cons (OneAnswer "fred")) $
    prologWrite "house" $ return NoMoreAnswers

testProg2 :: TestProgN
testProg2 topLevelCont = do
  y <- prologRead topLevelCont "input1? "
  prologWrite y $ prologWrite "---" $ do
    z <- prologRead topLevelCont "input2? "
    return $! (OneAnswer y) `Cons` (OneAnswer z) `Cons` NoMoreAnswers

testProg3 :: TestProgN
testProg3 topLevelCont = do
  y <- prologRead topLevelCont "input1? "
  prologWrite y $ do
    z <- prologRead topLevelCont "input2? "
    prologWrite "---" $
      return $! (OneAnswer y) `Cons` (OneAnswer z) `Cons` NoMoreAnswers


