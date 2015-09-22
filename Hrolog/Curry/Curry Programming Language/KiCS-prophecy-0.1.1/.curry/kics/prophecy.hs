{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Main (module Main) where

import Curry.RunTimeSystem
import Curry.Module.Prelude
import Curry.Module.Transform



-- begin included



-- end included

main  = (Prelude.>>)(Curry.RunTimeSystem.setProgNameAndOrBased("Transform"))(Curry.Module.Prelude.curryIOVoid(Curry.Module.Transform.c_aux2))


