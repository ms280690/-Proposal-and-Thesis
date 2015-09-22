
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}


-- Mehul Solanki.

-- Unification Monad using the librabry monad-unify.

import Control.Monad.Unify
import Data.Char
import Data.String.Utils
import Data.HashMap.Strict (empty, insert)

instance Partial String where

	unknown u = show u

	isUnknown s = if (foldr (&&) True (map isDigit s)) 
		then Just (read s :: Int) 
		else Nothing

	unknowns s = map (read) (split "," s) 	

	($?) (Substitution subs) s =  s

--callUnknown :: Unknown -> String
--callUnknown u = unknown u


--instance Partial String => Unifiable Int String  where
--	(=?=) s1 s2 =


instance Show Substitution String where
 	func =                       