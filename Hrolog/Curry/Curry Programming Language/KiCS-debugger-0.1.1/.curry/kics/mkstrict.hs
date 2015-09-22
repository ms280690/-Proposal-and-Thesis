{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Main (module Main) where

import Curry.RunTimeSystem
import Curry.Module.Prelude
import Curry.Module.TransformationDependencies



-- begin included



-- end included

main  = (Prelude.>>)(Curry.RunTimeSystem.setProgNameAndOrBased("TransformationDependencies"))(Curry.Module.Prelude.curryIOVoid(Curry.Module.TransformationDependencies.c_aux2))


