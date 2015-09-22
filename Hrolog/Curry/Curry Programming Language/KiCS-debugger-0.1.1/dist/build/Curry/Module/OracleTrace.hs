{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleTrace (module Curry.Module.OracleTrace) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Trace
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_trace :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_trace x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)


