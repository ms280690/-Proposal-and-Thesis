
-- Mehul Solanki.

-- Unification Monad using the librabry monad-unify.



{--
:set -XTypeSynonymInstances

:set -XFlexibleInstances

:set -XInstanceSigs
--}

import Control.Monad.Unify


instance Partial String where
	
	unknown u = show u

	isUnknown :: String -> Maybe Unknown
	isUnknown s = if ((read s) :: Int) > 0 then Just ((read s) :: Int) else Nothing 
	
	unknowns :: String -> [Unknown]
		
	($?) :: Substitution String -> String -> String





	