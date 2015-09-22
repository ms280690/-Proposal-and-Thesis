-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
-- ScopeEnv - provides functions and data types for dealing with nested
--            scope environments to store data from nested scopes
--
-- This module should be imported using "import qualified" to avoid name
-- clashes
--
-- November 2005,
-- Martin Engelke (men@informatik.uni-kiel.de)
--
module ScopeEnv (ScopeEnv,
		 new, insert, update, modify, lookup, sureLookup,
		 level, exists, beginScope, endScope, endScopeUp,
		 toList, toLevelList, currentLevel) where

import qualified Data.Map as Map
import Prelude hiding (lookup)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Data type for representing information in nested scopes.
data ScopeEnv a b = ScopeEnv Int (Map.Map a (b,Int)) [Map.Map a (b,Int)]
		    deriving Show


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Returns an empty scope environment
new :: Ord a => ScopeEnv a b
new = ScopeEnv 0 Map.empty []


-- Inserts a value under a key into the environment of the current scope
insert :: Ord a => a -> b -> ScopeEnv a b -> ScopeEnv a b
insert key val env = modifySE insertLev env
 where
 insertLev lev local = Map.insert key (val,lev) local


-- Updates the value stored under an existing key in the environment of 
-- the current scope
update :: Ord a => a -> b -> ScopeEnv a b -> ScopeEnv a b
update key val env = modifySE updateLev env
 where
 updateLev lev local = maybe local 
		             (\ (_,lev') ->  Map.insert key (val,lev') local)
			     (Map.lookup key local)

-- Modifies the value of an existing key by applying the function 'fun'
-- in the environment of the current scope
modify :: Ord a => (b -> b) -> a -> ScopeEnv a b -> ScopeEnv a b
modify fun key env = modifySE modifyLev env
 where
 modifyLev lev local 
    = maybe local
            (\ (val',lev') -> Map.insert key (fun val', lev') local)
	    (Map.lookup key local)


-- Looks up the value which is stored under a key from the environment of
-- the current scope
lookup :: Ord a => a -> ScopeEnv a b -> Maybe b
lookup key env = selectSE lookupLev env
 where
 lookupLev lev local = maybe Nothing (Just . fst) (Map.lookup key local)


-- Similar to 'lookup', but returns an alternative value, if the key
-- doesn't exist in the environment of the current scope
sureLookup :: Ord a => a -> b -> ScopeEnv a b -> b
sureLookup key alt env = maybe alt id (lookup key env)


-- Returns the level of the last insertion of a key
level :: Ord a => a -> ScopeEnv a b -> Int
level key env = selectSE levelLev env
 where
 levelLev lev local = maybe (-1) snd (Map.lookup key local)


-- Checks, whether a key exists in the environment of the current scope
exists :: Ord a => a -> ScopeEnv a b -> Bool
exists key env = selectSE existsLev env
 where
 existsLev lev local = maybe False (const True) (Map.lookup key local)


-- Switches to the next scope (i.e. pushes the environment of the current
-- scope onto the top of an scope stack and increments the level counter)
beginScope :: Ord a => ScopeEnv a b -> ScopeEnv a b
beginScope (ScopeEnv lev top [])
   = ScopeEnv (lev + 1) top [top]
beginScope (ScopeEnv lev top (local:locals))
   = ScopeEnv (lev + 1) top (local:local:locals)


-- Switches to the previous scope (i.e. pops the environment from the top
-- of the scope stack and decrements the level counter)
endScope :: Ord a => ScopeEnv a b -> ScopeEnv a b
endScope (ScopeEnv _ top [])
   = ScopeEnv 0 top []
endScope (ScopeEnv lev top (_:locals))
   = ScopeEnv (lev - 1) top locals


-- Behaves like 'endScope' but additionally updates the environment of
-- the previous scope by updating all keys with the corresponding values
-- from the poped environment
endScopeUp :: Ord a => ScopeEnv a b -> ScopeEnv a b
endScopeUp (ScopeEnv _ top [])
   = ScopeEnv 0 top []
endScopeUp (ScopeEnv lev top (local:[]))
   = ScopeEnv 0 (foldr (updateSE local) top (Map.toList top)) []
endScopeUp (ScopeEnv lev top (local:local':locals))
   = ScopeEnv (lev - 1) 
              top 
	      ((foldr (updateSE local) local' (Map.toList local')):locals)


-- Returns the environment of current scope as a (key,value) list
toList :: Ord a => ScopeEnv a b -> [(a,b)]
toList env = selectSE toListLev env
 where
 toListLev lev local = map (\ (key,(val,_)) -> (key,val)) (Map.toList local)


-- Returns all (key,value) pairs from the environment of the current scope 
-- which has been inserted in the current level
toLevelList :: Ord a => ScopeEnv a b -> [(a,b)]
toLevelList env = selectSE toLevelListLev env
 where
 toLevelListLev lev local
    = map (\ (key,(val,_)) -> (key,val))
          (filter (\ (_,(_,lev')) -> lev' == lev) (Map.toList local))


-- Returns the current level
currentLevel :: Ord a => ScopeEnv a b -> Int
currentLevel env = selectSE const env


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Privates...

--
modifySE :: (Int -> Map.Map a (b,Int) -> Map.Map a (b,Int)) -> ScopeEnv a b 
          -> ScopeEnv a b
modifySE f (ScopeEnv _ top []) 
   = ScopeEnv 0 (f 0 top) []
modifySE f (ScopeEnv lev top (local:locals))
   = ScopeEnv lev top ((f lev local):locals)

--
selectSE :: (Int -> Map.Map a (b,Int) -> c) -> ScopeEnv a b -> c
selectSE f (ScopeEnv _ top [])        = f 0 top
selectSE f (ScopeEnv lev _ (local:_)) = f lev local

--
updateSE :: Ord a => Map.Map a (b,Int) -> (a,(b,Int)) ->  Map.Map a (b,Int) 
          -> Map.Map a (b,Int)
updateSE local (key,(_,lev)) local'
   = maybe local' 
           (\ (val',lev') 
	    -> if lev == lev' then Map.insert key (val',lev) local' 
                              else local')
	   (Map.lookup key local)



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
