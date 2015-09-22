
-- STRef Experiments
import STVarExperiment

import Data.STRef
import Control.Monad.ST

import Control.Unification
import Control.Unification.STVar as ST
import Control.Unification.IntVar
import Control.Unification.Ranked.STVar
import Control.Unification.Types

myFunc1 = print $ runST $ do 
	x <- newSTRef "a"
	readSTRef x 
{--
--myFunc2 :: IO ()
myFunc2 = print $ runSTBinding $ do
	x <- freeVar
	lookupVar x

--}



 