module Curry.Debugger.Logic where

import Data.Generics
import Data.Maybe (fromJust)
import Data.Map as Map

type OrRef = Integer
type Bind = Int
type ConstraintStore = Map.Map OrRef Bind

emptyConstraintStore :: ConstraintStore
emptyConstraintStore = Map.empty

lookupRef :: ConstraintStore -> OrRef -> Maybe Bind
lookupRef = flip Map.lookup

insertBind :: ConstraintStore -> OrRef -> Bind -> ConstraintStore
insertBind cs r b = Map.insert r b cs
