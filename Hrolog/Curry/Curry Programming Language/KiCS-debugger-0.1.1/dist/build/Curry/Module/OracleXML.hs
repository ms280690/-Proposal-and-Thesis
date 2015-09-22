{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleXML (module Curry.Module.OracleXML) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.XML
import Curry.Module.Char
import Curry.Module.List
import Curry.Module.Prelude
import Curry.Module.Read
import Curry.Module.OracleChar
import Curry.Module.OracleList
import Curry.Module.OraclePrelude
import Curry.Module.OracleRead



-- begin included



-- end included

c_encoding2Attribute :: Curry.Module.XML.C_Encoding -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_encoding2Attribute x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_121(x2)(x1)(st))(st)



c_encoding2EncFunc :: Curry.Module.XML.C_Encoding -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_encoding2EncFunc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_120(x2)(x1)(st))(st)



c_standardEncoding :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_standardEncoding x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_119(x2)(x1)(st))(st)



c_iso88591Encoding :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_iso88591Encoding x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_110(x2)(x1)(st))(st)



c_iso88591list :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_iso88591list x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))(st)



c_lookupEncoding :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_Encoding
c_lookupEncoding x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_108(x2)(x1)(st))(st)



c_lookupDtdUrl :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lookupDtdUrl x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_106(x2)(x1)(st))(st)



c_hasDtdUrl :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_hasDtdUrl x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_104(x2)(x1)(st))(st)



c_xtxt :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xtxt x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.XML.C_XText(x2))(st)



c_xml :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xml x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.XML.C_XElem(x2)(Curry.Module.Prelude.List)(x3))(st)



c_writeXmlFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_writeXmlFile x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_writeXmlFileWithParams(x2)((Curry.Module.Prelude.:<)(Curry.Module.XML.C_Enc(Curry.Module.XML.C_StandardEnc))(Curry.Module.Prelude.List))(x3)(x1)(st))(st)



c_writeXmlFileWithParams :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_writeXmlFileWithParams x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_writeFile(x2)(Curry.Module.OracleXML.c_showXmlDocWithParams(x3)(x4)(x1)(st))(x5)(st))(st)



c_showXmlDoc :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlDoc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_showXmlDocWithParams(Curry.Module.Prelude.List)(x2)(x1)(st))(st)



c_showXmlDocWithParams :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlDocWithParams x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_102(x2)(x3)(x1)(st))(st)



c_showXmlExp :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlExp x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_99(x2)(x3)(x4)(x1)(st))(st)



c_showXmlExp'46_'35selFP3'35s :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlExp'46_'35selFP3'35s x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_96(x2)(x1)(st))(st)



c_xtab :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xtab x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_take(x2)(Curry.Module.OraclePrelude.c_repeat(Curry.Module.Prelude.C_Char(' '))(x1)(st))(x3)(st))(st)



c_showXmlOpenTag :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t0)) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlOpenTag x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x2)(Curry.Module.OraclePrelude.c_concat(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleXML.c_showXmlOpenTag'46attr2string'46125(x4)))))(x1)(st))(x3)(x5)(st))(x6)(st))(x7)(st))(x8)(st))(st)



c_showXmlOpenTag'46attr2string'46125 :: (Curry t130) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t130 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t130) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlOpenTag'46attr2string'46125 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_93(x2)(x3)(x1)(st))(st)



c_showXmlExps :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlExps x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleXML.c_showXmlExp(x2)(x4)))))(x1)(st))(x3)(x5)(st))(st)



c_isXText :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isXText x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_92(x2)(x1)(st))(st)



c_xmlUnquoteSpecials :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xmlUnquoteSpecials x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_91(x2)(x1)(st))(st)



c_xmlUnquoteSpecials'46_'35selFP5'35special :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xmlUnquoteSpecials'46_'35selFP5'35special x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_88(x2)(x1)(st))(st)



c_xmlUnquoteSpecials'46_'35selFP6'35rest :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xmlUnquoteSpecials'46_'35selFP6'35rest x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_87(x2)(x1)(st))(st)



c_xmlUnquoteSpecial :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xmlUnquoteSpecial x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_86(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))(x1)(st))(x4)(st))(st)



c_unquoteUnicode :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unquoteUnicode x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_73(x2)(x1)(st))(st)



c_readXmlFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.XML.C_XmlExp))
c_readXmlFile x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_readFile(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleXML.c_readXmlFile'46_'35lambda3(x2)))))(x3)(st))(st)



c_readXmlFile'46_'35lambda3 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.XML.C_XmlExp))
c_readXmlFile'46_'35lambda3 x2 x3 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleXML.c_parseXmlString(x3)(x1)(st)} in let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_68(x2)(x4)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.List)(x5)(st))(x6)(st))(st))(st)



c_readUnsafeXmlFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.XML.C_XmlExp)))
c_readUnsafeXmlFile x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_catchFail(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleXML.c_readXmlFile(x2)(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))))(x3)(st))(x4)(st))(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.C_Nothing)(x5)(st))(x6)(st))(st)



c_showXmlFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_showXmlFile x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleXML.c_readXmlFile(x2)(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_putStr))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleXML.c_showXmlDoc))))(x3)(st))(x4)(st))(st)



c_readFileWithXmlDocs :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)))
c_readFileWithXmlDocs x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_readFile(x2)(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleXML.c_parseXmlString))))(x3)(st))(x4)(st))(st)



c_parseXmlString :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlString x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_fst(Curry.Module.OracleXML.c_parseXmlTokens(Curry.Module.OracleXML.c_scanXmlString(x2)(x1)(st))(Curry.Module.Prelude.C_Nothing)(x3)(st))(x4)(st))(st)



c_parseXmlTokens :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)
c_parseXmlTokens x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_66(x3)(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP8'35xexps :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP8'35xexps x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_58(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP9'35rem_xtokens :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP9'35rem_xtokens x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_57(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP14'35xexps1 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP14'35xexps1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_56(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP15'35xtokens1 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP15'35xtokens1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_55(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP12'35xexps :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP12'35xexps x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_54(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP13'35rem_xtokens :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP13'35rem_xtokens x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_53(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP17'35xexps :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP17'35xexps x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_52(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP18'35rem_xtokens :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP18'35rem_xtokens x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_51(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP20'35xexps :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP20'35xexps x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_50(x2)(x1)(st))(st)



c_parseXmlTokens'46_'35selFP21'35rem_xtokens :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP21'35rem_xtokens x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_49(x2)(x1)(st))(st)



c_scanXmlString :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlString x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c_scanXmlString'46scanXml'46191(Curry.Module.Oracle.c_apply(Curry.Module.OracleXML.c_dropBlanks(x1)(st))(x2)(x3)(st))(x4)(st))(st)



c_scanXmlString'46scanXml'46191 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlString'46scanXml'46191 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_48(x2)(x1)(st))(st)



c_scanXmlString'46scanXml'46191'46_'35selFP23'35initxt :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlString'46scanXml'46191'46_'35selFP23'35initxt x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_46(x2)(x1)(st))(st)



c_scanXmlString'46scanXml'46191'46_'35selFP24'35remtag :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlString'46scanXml'46191'46_'35selFP24'35remtag x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_45(x2)(x1)(st))(st)



c_scanXmlText :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_scanXmlText x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_44(x2)(x1)(st))(st)



c_scanXmlText'46_'35selFP26'35txt :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlText'46_'35selFP26'35txt x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_39(x2)(x1)(st))(st)



c_scanXmlText'46_'35selFP27'35rem :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlText'46_'35selFP27'35rem x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_38(x2)(x1)(st))(st)



c_scanXmlText'46_'35selFP29'35txt :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlText'46_'35selFP29'35txt x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_37(x2)(x1)(st))(st)



c_scanXmlText'46_'35selFP30'35rem :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlText'46_'35selFP30'35rem x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_36(x2)(x1)(st))(st)



c_scanXmlElem :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlElem x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_35(x2)(x1)(st))(st)



c_scanXmlElemName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlElemName x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_30(x2)(x3)(x1)(st))(st)



c_scanXmlElemName'46_'35selFP32'35attrs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_scanXmlElemName'46_'35selFP32'35attrs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_24(x2)(x1)(st))(st)



c_scanXmlElemName'46_'35selFP33'35rest :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlElemName'46_'35selFP33'35rest x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_23(x2)(x1)(st))(st)



c_scanXmlComment :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlComment x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_22(x2)(x1)(st))(st)



c_scanXmlCData :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlCData x2 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(let {x3 = Curry.Module.OracleXML.c_dropCData(x2)(x1)(st)} in let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c__case_20(x3)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_head(x3)(x4)(st))(Curry.Module.Prelude.C_Char('>'))(x5)(st))(x6)(st))(st))(st)



c_dropCData :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_dropCData x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_19(x2)(x1)(st))(st)



c_scanXmlProcInstr :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlProcInstr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_15(x2)(x1)(st))(st)



c_parseAttrs :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_parseAttrs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_13(x2)(x1)(st))(st)



c_parseAttrs'46_'35selFP41'35name :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP41'35name x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_10(x2)(x1)(st))(st)



c_parseAttrs'46_'35selFP42'35rest1 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP42'35rest1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_9(x2)(x1)(st))(st)



c_parseAttrs'46_'35selFP39'35value :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP39'35value x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_8(x2)(x1)(st))(st)



c_parseAttrs'46_'35selFP40'35rest2 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP40'35rest2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_7(x2)(x1)(st))(st)



c_parseAttrs'46_'35selFP37'35rem_attrs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_parseAttrs'46_'35selFP37'35rem_attrs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_6(x2)(x1)(st))(st)



c_parseAttrs'46_'35selFP38'35rem_inp :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP38'35rem_inp x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_5(x2)(x1)(st))(st)



c_dropBlanks :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_dropBlanks x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_dropWhile(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleChar.c_isSpace))))))))(st)



c_splitAtChar :: (Curry t0) => t0 -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_splitAtChar x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_4(x2)(x3)(x1)(st))(st)



c_splitAtChar'46_'35selFP44'35first :: (Curry t231) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t231) (Curry.Module.Prelude.List t231)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t231
c_splitAtChar'46_'35selFP44'35first x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_2(x2)(x1)(st))(st)



c_splitAtChar'46_'35selFP45'35rest :: (Curry t231) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t231) (Curry.Module.Prelude.List t231)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t231
c_splitAtChar'46_'35selFP45'35rest x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_1(x2)(x1)(st))(st)



c_textOfXml :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_textOfXml x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_not))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_null))))(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleXML.c_textOfXml'46textOfXmlItem'46255))))(x2)(x3)(st))(x4)(st))(x5)(st))(x6)(st))(st)



c_textOfXml'46textOfXmlItem'46255 :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_textOfXml'46textOfXmlItem'46255 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_0(x2)(x1)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_0_case__121(x1)(x2)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_1_case__120(x1)(x2)(st))(st)



c__case_2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_2_case__119(x1)(x2)(st))(st)



c__case_4 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_4_case__118(x1)(x2)(x3)(st))(st)



c__case_3 x2 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_3_case__117(x1)(x2)(x4)(x5)(x9)(st))(st)



c__case_5 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_5_case__116(x1)(x2)(st))(st)



c__case_6 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_6_case__115(x1)(x2)(st))(st)



c__case_7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_7_case__114(x1)(x2)(st))(st)



c__case_8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_8_case__113(x1)(x2)(st))(st)



c__case_9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_9_case__112(x1)(x2)(st))(st)



c__case_10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_10_case__111(x1)(x2)(st))(st)



c__case_13 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_13_case__110(x1)(x2)(st))(st)



c__case_12 x3 x4 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_12_case__109(x1)(x3)(x4)(x14)(st))(st)



c__case_11 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_11_case__108(x1)(x3)(x4)(x5)(st))(st)



c__case_15 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_15_case__107(x1)(x2)(st))(st)



c__case_14 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_14_case__106(x1)(x4)(x5)(st))(st)



c__case_19 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_19_case__105(x1)(x2)(st))(st)



c__case_18 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_18_case__104(x1)(x3)(x4)(x5)(st))(st)



c__case_17 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_17_case__103(x1)(x3)(x4)(x5)(st))(st)



c__case_16 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_16_case__102(x1)(x4)(x5)(st))(st)



c__case_20 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_20_case__101(x1)(x3)(x4)(st))(st)



c__case_22 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_22_case__100(x1)(x2)(st))(st)



c__case_21 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_21_case__99(x1)(x4)(x5)(st))(st)



c__case_23 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_23_case__98(x1)(x2)(st))(st)



c__case_24 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_24_case__97(x1)(x2)(st))(st)



c__case_30 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_30_case__96(x1)(x2)(x3)(st))(st)



c__case_29 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_29_case__95(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_28 x2 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_28_case__94(x1)(x2)(x4)(x5)(x9)(st))(st)



c__case_26 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_26_case__93(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_25 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_25_case__92(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_27 x2 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_27_case__91(x1)(x2)(x7)(x8)(x9)(st))(st)



c__case_35 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_35_case__90(x1)(x2)(st))(st)



c__case_34 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_34_case__89(x1)(x3)(x4)(x5)(st))(st)



c__case_32 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_32_case__88(x1)(x3)(x4)(x5)(st))(st)



c__case_31 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_31_case__87(x1)(x3)(x4)(x5)(st))(st)



c__case_33 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_33_case__86(x1)(x4)(x5)(st))(st)



c__case_36 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_36_case__85(x1)(x2)(st))(st)



c__case_37 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_37_case__84(x1)(x2)(st))(st)



c__case_38 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_38_case__83(x1)(x2)(st))(st)



c__case_39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_39_case__82(x1)(x2)(st))(st)



c__case_44 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_44_case__81(x1)(x2)(st))(st)



c__case_43 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_43_case__80(x1)(x3)(x4)(x5)(st))(st)



c__case_42 x3 x4 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_42_case__79(x1)(x3)(x4)(x8)(st))(st)



c__case_40 x3 x4 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_40_case__78(x1)(x3)(x4)(x11)(st))(st)



c__case_41 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_41_case__77(x1)(x6)(x7)(st))(st)



c__case_45 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_45_case__76(x1)(x2)(st))(st)



c__case_46 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_46_case__75(x1)(x2)(st))(st)



c__case_48 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_48_case__74(x1)(x2)(st))(st)



c__case_47 x3 x4 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_47_case__73(x1)(x3)(x4)(x8)(st))(st)



c__case_49 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_49_case__72(x1)(x2)(st))(st)



c__case_50 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_50_case__71(x1)(x2)(st))(st)



c__case_51 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_51_case__70(x1)(x2)(st))(st)



c__case_52 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_52_case__69(x1)(x2)(st))(st)



c__case_53 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_53_case__68(x1)(x2)(st))(st)



c__case_54 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_54_case__67(x1)(x2)(st))(st)



c__case_55 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_55_case__66(x1)(x2)(st))(st)



c__case_56 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_56_case__65(x1)(x2)(st))(st)



c__case_57 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_57_case__64(x1)(x2)(st))(st)



c__case_58 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_58_case__63(x1)(x2)(st))(st)



c__case_66 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_66_case__62(x1)(x3)(x2)(st))(st)



c__case_64 x3 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_64_case__61(x1)(x3)(x5)(x4)(st))(st)



c__case_63 x3 x5 x11 x12 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_63_case__60(x1)(x3)(x5)(x11)(x12)(x10)(st))(st)



c__case_62 x3 x5 x11 x12 x13 x14 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_62_case__59(x1)(x3)(x5)(x11)(x12)(x13)(x14)(x21)(st))(st)



c__case_61 x3 x5 x11 x12 x13 x14 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_61_case__58(x1)(x3)(x5)(x11)(x12)(x13)(x14)(x15)(st))(st)



c__case_59 x3 x5 x11 x12 x13 x14 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_59_case__57(x1)(x3)(x5)(x11)(x12)(x13)(x14)(x27)(st))(st)



c__case_60 x3 x5 x11 x12 x14 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_60_case__56(x1)(x3)(x5)(x11)(x12)(x14)(x24)(st))(st)



c__case_65 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_65_case__55(x1)(x3)(st))(st)



c__case_68 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_68_case__54(x1)(x2)(x4)(x5)(st))(st)



c__case_67 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_67_case__53(x1)(x2)(x4)(x5)(st))(st)



c__case_73 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_73_case__52(x1)(x2)(st))(st)



c__case_72 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_72_case__51(x1)(x3)(x4)(x5)(st))(st)



c__case_69 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_69_case__50(x1)(x3)(x4)(x5)(st))(st)



c__case_71 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_71_case__49(x1)(x4)(st))(st)



c__case_70 x4 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_70_case__48(x1)(x4)(x6)(x7)(st))(st)



c__case_86 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_86_case__47(x1)(x2)(x3)(x4)(st))(st)



c__case_85 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_85_case__46(x1)(x2)(x3)(x4)(st))(st)



c__case_84 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_84_case__45(x1)(x2)(x3)(x4)(st))(st)



c__case_83 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_83_case__44(x1)(x2)(x3)(x4)(st))(st)



c__case_82 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_82_case__43(x1)(x2)(x3)(x4)(st))(st)



c__case_81 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_81_case__42(x1)(x2)(x3)(x4)(st))(st)



c__case_80 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_80_case__41(x1)(x2)(x3)(x4)(st))(st)



c__case_79 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_79_case__40(x1)(x2)(x3)(x4)(st))(st)



c__case_78 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_78_case__39(x1)(x2)(x3)(x4)(st))(st)



c__case_77 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_77_case__38(x1)(x2)(x3)(x4)(st))(st)



c__case_76 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_76_case__37(x1)(x2)(x3)(x4)(st))(st)



c__case_75 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_75_case__36(x1)(x2)(x3)(x4)(st))(st)



c__case_74 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_74_case__35(x1)(x2)(x3)(x4)(st))(st)



c__case_87 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_87_case__34(x1)(x2)(st))(st)



c__case_88 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_88_case__33(x1)(x2)(st))(st)



c__case_91 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_91_case__32(x1)(x2)(st))(st)



c__case_90 x3 x4 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_90_case__31(x1)(x3)(x4)(x8)(st))(st)



c__case_89 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_89_case__30(x1)(x3)(x4)(x5)(st))(st)



c__case_92 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_92_case__29(x1)(x2)(st))(st)



c__case_93 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_93_case__28(x1)(x2)(x3)(st))(st)



c__case_96 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_96_case__27(x1)(x2)(st))(st)



c__case_95 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_95_case__26(x1)(x4)(x3)(st))(st)



c__case_94 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_94_case__25(x1)(x5)(x4)(st))(st)



c__case_99 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_99_case__24(x1)(x2)(x3)(x4)(st))(st)



c__case_98 x2 x3 x6 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_98_case__23(x1)(x2)(x3)(x6)(x8)(x9)(st))(st)



c__case_97 x2 x3 x6 x8 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_97_case__22(x1)(x2)(x3)(x6)(x8)(x11)(st))(st)



c__case_102 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_102_case__21(x1)(x2)(x3)(st))(st)



c__case_100 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_100_case__20(x1)(x2)(x4)(x5)(st))(st)



c__case_101 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_101_case__19(x1)(x3)(st))(st)



c__case_104 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_104_case__18(x1)(x2)(st))(st)



c__case_103 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_103_case__17(x1)(x4)(x3)(st))(st)



c__case_106 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_106_case__16(x1)(x2)(st))(st)



c__case_105 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_105_case__15(x1)(x4)(x3)(st))(st)



c__case_108 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_108_case__14(x1)(x2)(st))(st)



c__case_107 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_107_case__13(x1)(x4)(x3)(st))(st)



c__case_110 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_110_case__12(x1)(x2)(st))(st)



c__case_109 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_109_case__11(x1)(x3)(x4)(x5)(st))(st)



c__case_119 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_119_case__10(x1)(x2)(st))(st)



c__case_118 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_118_case__9(x1)(x3)(x4)(x5)(st))(st)



c__case_117 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_117_case__8(x1)(x3)(x4)(x5)(st))(st)



c__case_116 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_116_case__7(x1)(x3)(x4)(x5)(st))(st)



c__case_115 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_115_case__6(x1)(x3)(x4)(x5)(st))(st)



c__case_114 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_114_case__5(x1)(x3)(x4)(x5)(st))(st)



c__case_113 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_113_case__4(x1)(x3)(x4)(x5)(st))(st)



c__case_112 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_112_case__3(x1)(x3)(x4)(x5)(st))(st)



c__case_111 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_111_case__2(x1)(x3)(x4)(x5)(st))(st)



c__case_120 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_120_case__1(x1)(x2)(st))(st)



c__case_121 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_121_case__0(x1)(x2)(st))(st)



c__case_121_case__0 x1 x2@Curry.Module.XML.C_StandardEnc st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_121_case__0 x1 x2@Curry.Module.XML.C_Iso88591Enc st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('8'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('8'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('5'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('9'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))(st)
c__case_121_case__0 x1 (Curry.Module.XML.C_EncodingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_121_case__0(x1)(x)(st))(i)(xs)(st)
c__case_121_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_121_case__0")(x)



c__case_120_case__1 x1 x2@Curry.Module.XML.C_StandardEnc st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleXML.c_standardEncoding))))(st)
c__case_120_case__1 x1 x2@Curry.Module.XML.C_Iso88591Enc st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleXML.c_iso88591Encoding))))(st)
c__case_120_case__1 x1 (Curry.Module.XML.C_EncodingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_120_case__1(x1)(x)(st))(i)(xs)(st)
c__case_120_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_120_case__1")(x)



c__case_111_case__2 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.OracleXML.c_standardEncoding(x4)(x1)(st)))(st)
c__case_111_case__2 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_111_case__2 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_111_case__2(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_111_case__2 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_111_case__2")(x)



c__case_112_case__3 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(Curry.Module.OraclePrelude.c_ord(x3)(x1)(st))(x6)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_standardEncoding(x4)(x7)(st))(x8)(st))(x9)(st))(x10)(st))(st)
c__case_112_case__3 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_111(x3)(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x11)(st))(st)
c__case_112_case__3 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_112_case__3(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_112_case__3 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_112_case__3")(x)



c__case_113_case__4 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(Curry.Module.OraclePrelude.c_ord(x3)(x1)(st))(x6)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_standardEncoding(x4)(x7)(st))(x8)(st))(x9)(st))(x10)(st))(st)
c__case_113_case__4 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c__case_112(x3)(x4)(Curry.Module.OraclePrelude.op_62(Curry.Module.OraclePrelude.c_ord(x3)(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(x11)(st))(x12)(st))(st)
c__case_113_case__4 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_113_case__4(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_113_case__4 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_113_case__4")(x)



c__case_114_case__5 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List)))))))(Curry.Module.OracleXML.c_standardEncoding(x4)(x1)(st))(x6)(st))(st)
c__case_114_case__5 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c__case_113(x3)(x4)(Curry.Module.OraclePrelude.op_60(Curry.Module.OraclePrelude.c_ord(x3)(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))(x7)(st))(x8)(st))(st)
c__case_114_case__5 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_114_case__5(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_114_case__5 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_114_case__5")(x)



c__case_115_case__6 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List)))))))(Curry.Module.OracleXML.c_standardEncoding(x4)(x1)(st))(x6)(st))(st)
c__case_115_case__6 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_114(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(x7)(st))(st)
c__case_115_case__6 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_115_case__6(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_115_case__6 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_115_case__6")(x)



c__case_116_case__7 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))))))(Curry.Module.OracleXML.c_standardEncoding(x4)(x1)(st))(x6)(st))(st)
c__case_116_case__7 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_115(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('\"'))(x1)(st))(x7)(st))(st)
c__case_116_case__7 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_116_case__7(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_116_case__7 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_116_case__7")(x)



c__case_117_case__8 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List)))))(Curry.Module.OracleXML.c_standardEncoding(x4)(x1)(st))(x6)(st))(st)
c__case_117_case__8 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_116(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('&'))(x1)(st))(x7)(st))(st)
c__case_117_case__8 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_117_case__8(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_117_case__8 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_117_case__8")(x)



c__case_118_case__9 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List)))))(Curry.Module.OracleXML.c_standardEncoding(x4)(x1)(st))(x6)(st))(st)
c__case_118_case__9 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_117(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('>'))(x1)(st))(x7)(st))(st)
c__case_118_case__9 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_118_case__9(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_118_case__9 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_118_case__9")(x)



c__case_119_case__10 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_119_case__10 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_118(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('<'))(x1)(st))(x5)(st))(st)
c__case_119_case__10 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_119_case__10(x1)(x)(st))(i)(xs)(st)
c__case_119_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_119_case__10")(x)



c__case_109_case__11 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.OracleXML.c_iso88591Encoding(x4)(x1)(st)))(st)
c__case_109_case__11 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c_standardEncoding((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.OracleXML.c_iso88591Encoding(x4)(x6)(st))(x7)(st))(st)
c__case_109_case__11 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_109_case__11(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_109_case__11 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_109_case__11")(x)



c__case_110_case__12 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_110_case__12 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.OracleXML.c__case_109(x3)(x4)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(Curry.Module.OraclePrelude.c_ord(x3)(x1)(st))(x5)(st))(Curry.Module.OracleXML.c_iso88591list(x6)(st))(x7)(st))(x8)(st))(st)
c__case_110_case__12 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_110_case__12(x1)(x)(st))(i)(xs)(st)
c__case_110_case__12 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_110_case__12")(x)



c__case_107_case__13 x1 x4 x3@(Curry.Module.XML.C_Enc x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_107_case__13 x1 x4 x3@(Curry.Module.XML.C_DtdUrl x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_lookupEncoding(x4)(x1)(st))(st)
c__case_107_case__13 x1 x4 (Curry.Module.XML.C_XmlDocParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_107_case__13(x1)(x4)(x)(st))(i)(xs)(st)
c__case_107_case__13 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_107_case__13")(x)



c__case_108_case__14 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_107(x4)(x3)(x1)(st))(st)
c__case_108_case__14 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.XML.C_StandardEnc)(st)
c__case_108_case__14 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_108_case__14(x1)(x)(st))(i)(xs)(st)
c__case_108_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_108_case__14")(x)



c__case_105_case__15 x1 x4 x3@(Curry.Module.XML.C_Enc x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_lookupDtdUrl(x4)(x1)(st))(st)
c__case_105_case__15 x1 x4 x3@(Curry.Module.XML.C_DtdUrl x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_105_case__15 x1 x4 (Curry.Module.XML.C_XmlDocParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_105_case__15(x1)(x4)(x)(st))(i)(xs)(st)
c__case_105_case__15 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_105_case__15")(x)



c__case_106_case__16 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_105(x4)(x3)(x1)(st))(st)
c__case_106_case__16 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_106_case__16(x1)(x)(st))(i)(xs)(st)
c__case_106_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_106_case__16")(x)



c__case_103_case__17 x1 x4 x3@(Curry.Module.XML.C_DtdUrl x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_103_case__17 x1 x4 x3@(Curry.Module.XML.C_Enc x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_hasDtdUrl(x4)(x1)(st))(st)
c__case_103_case__17 x1 x4 (Curry.Module.XML.C_XmlDocParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_103_case__17(x1)(x4)(x)(st))(i)(xs)(st)
c__case_103_case__17 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_103_case__17")(x)



c__case_104_case__18 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_104_case__18 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_103(x4)(x3)(x1)(st))(st)
c__case_104_case__18 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_104_case__18(x1)(x)(st))(i)(xs)(st)
c__case_104_case__18 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_104_case__18")(x)



c__case_101_case__19 x1 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))(Curry.Module.Prelude.List)))(st)
c__case_101_case__19 x1 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))(st)
c__case_101_case__19 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_101_case__19(x1)(x)(st))(i)(xs)(st)
c__case_101_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_101_case__19")(x)



c__case_100_case__20 x1 x2 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('Y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))(Curry.Module.OraclePrelude.op_43_43(x4)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('Y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c_lookupDtdUrl(x2)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(x6)(st))(x7)(st))(x8)(st))(x9)(st))(st)
c__case_100_case__20 x1 x2 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_100_case__20 x1 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_100_case__20(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_100_case__20 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_100_case__20")(x)



c__case_102_case__21 x1 x2 x3@(Curry.Module.XML.C_XElem x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))))))))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c_encoding2Attribute(Curry.Module.OracleXML.c_lookupEncoding(x2)(x1)(st))(x7)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c__case_101(x2)(Curry.Module.OracleXML.c_hasDtdUrl(x2)(x8)(st))(x9)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c__case_100(x2)(x4)(Curry.Module.OracleXML.c_hasDtdUrl(x2)(x10)(st))(x11)(st))(Curry.Module.OracleXML.c_showXmlExp(Curry.Module.Prelude.C_Zero)(Curry.Module.OracleXML.c_encoding2EncFunc(Curry.Module.OracleXML.c_lookupEncoding(x2)(x12)(st))(x13)(st))(Curry.Module.XML.C_XElem(x4)(x5)(x6))(x14)(st))(x15)(st))(x16)(st))(x17)(st))(x18)(st))(x19)(st))(x20)(st))(st)
c__case_102_case__21 x1 x2 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_102_case__21(x1)(x2)(x)(st))(i)(xs)(st)
c__case_102_case__21 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_102_case__21")(x)



c__case_97_case__22 x1 x2 x3 x6 x8 x11@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(x3)(Curry.Module.OracleXML.c_showXmlExp'46_'35selFP3'35s(x8)(x1)(st))(x12)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(x13)(st))(x14)(st))(x15)(st))(x16)(st))(st))(st))(st)
c__case_97_case__22 x1 x2 x3 x6 x8 x11@Curry.Module.Prelude.C_False st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c_showXmlExps(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(x8)(x3)(x17)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c_xtab(x2)(x18)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(x19)(st))(x20)(st))(x21)(st))(x22)(st))(x23)(st))(st)
c__case_97_case__22 x1 x2 x3 x6 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_97_case__22(x1)(x2)(x3)(x6)(x8)(x)(st))(i)(xs)(st)
c__case_97_case__22 x1 x2 x3 x6 x8 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_97_case__22")(x)



c__case_98_case__23 x1 x2 x3 x6 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(st)
c__case_98_case__23 x1 x2 x3 x6 x8 x9@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))))))(Curry.Module.OracleXML.c__case_97(x2)(x3)(x6)(x8)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_length(x8)(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x10)(st))(Curry.Module.OracleXML.c_isXText(Curry.Module.OraclePrelude.c_head(x8)(x11)(st))(x12)(st))(x13)(st))(x14)(st))(st)
c__case_98_case__23 x1 x2 x3 x6 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_98_case__23(x1)(x2)(x3)(x6)(x8)(x)(st))(i)(xs)(st)
c__case_98_case__23 x1 x2 x3 x6 x8 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_98_case__23")(x)



c__case_99_case__24 x1 x2 x3 x4@(Curry.Module.XML.C_XText x5) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c_xtab(x2)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(x3)(x5)(x9)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x10)(st))(x11)(st))(st)
c__case_99_case__24 x1 x2 x3 x4@(Curry.Module.XML.C_XElem x6 x7 x8) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c_xtab(x2)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c_showXmlOpenTag(x6)(x7)(x3)(x12)(st))(Curry.Module.OracleXML.c__case_98(x2)(x3)(x6)(x8)(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.Prelude.List)(x13)(st))(x14)(st))(x15)(st))(x16)(st))(st)
c__case_99_case__24 x1 x2 x3 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_99_case__24(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_99_case__24 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_99_case__24")(x)



c__case_94_case__25 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_94_case__25 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_94_case__25(x1)(x5)(x)(st))(i)(xs)(st)
c__case_94_case__25 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_94_case__25")(x)



c__case_95_case__26 x1 x4 x3@(Curry.Module.XML.C_XText x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_94(x5)(x4)(x1)(st))(st)
c__case_95_case__26 x1 x4 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_95_case__26(x1)(x4)(x)(st))(i)(xs)(st)
c__case_95_case__26 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_95_case__26")(x)



c__case_96_case__27 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_95(x4)(x3)(x1)(st))(st)
c__case_96_case__27 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_96_case__27(x1)(x)(st))(i)(xs)(st)
c__case_96_case__27 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_96_case__27")(x)



c__case_93_case__28 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x4)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_93_case__28 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_93_case__28(x1)(x2)(x)(st))(i)(xs)(st)
c__case_93_case__28 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_93_case__28")(x)



c__case_92_case__29 x1 x2@(Curry.Module.XML.C_XText x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_92_case__29 x1 x2@(Curry.Module.XML.C_XElem x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_92_case__29 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_92_case__29(x1)(x)(st))(i)(xs)(st)
c__case_92_case__29 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_92_case__29")(x)



c__case_89_case__30 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x4)(x1)(st)))(st)
c__case_89_case__30 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_89_case__30 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_89_case__30(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_89_case__30 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_89_case__30")(x)



c__case_90_case__31 x1 x3 x4 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleXML.c_splitAtChar(Curry.Module.Prelude.C_Char(';'))(x4)(x1)(st)} in let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x11)(Curry.Module.OracleXML.c_xmlUnquoteSpecial(Curry.Module.OracleXML.c_xmlUnquoteSpecials'46_'35selFP5'35special(x5)(x9)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials'46_'35selFP6'35rest(x5)(x10)(st))(x11)(st))(st))(st))(st))(st)
c__case_90_case__31 x1 x3 x4 x8@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_89(x3)(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x12)(st))(st)
c__case_90_case__31 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_90_case__31(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_90_case__31 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_90_case__31")(x)



c__case_91_case__32 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_91_case__32 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_90(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('&'))(x1)(st))(x5)(st))(st)
c__case_91_case__32 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_91_case__32(x1)(x)(st))(i)(xs)(st)
c__case_91_case__32 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_91_case__32")(x)



c__case_88_case__33 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_88_case__33 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_88_case__33(x1)(x)(st))(i)(xs)(st)
c__case_88_case__33 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_88_case__33")(x)



c__case_87_case__34 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_87_case__34 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_87_case__34(x1)(x)(st))(i)(xs)(st)
c__case_87_case__34 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_87_case__34")(x)



c__case_74_case__35 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleXML.c_unquoteUnicode(x2)(x1)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x5)(st))(x6)(st))(st)
c__case_74_case__35 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_74_case__35 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_74_case__35(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_74_case__35 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_74_case__35")(x)



c__case_75_case__36 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x5)(st)))(st)
c__case_75_case__36 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_74(x2)(x3)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x6)(st))(st)
c__case_75_case__36 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_75_case__36(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_75_case__36 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_75_case__36")(x)



c__case_76_case__37 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x5)(st)))(st)
c__case_76_case__37 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_75(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('z'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))))(x1)(st))(x6)(st))(st)
c__case_76_case__37 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_76_case__37(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_76_case__37 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_76_case__37")(x)



c__case_77_case__38 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x5)(st)))(st)
c__case_77_case__38 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_76(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(x1)(st))(x6)(st))(st)
c__case_77_case__38 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_77_case__38(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_77_case__38 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_77_case__38")(x)



c__case_78_case__39 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x5)(st)))(st)
c__case_78_case__39 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_77(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(x1)(st))(x6)(st))(st)
c__case_78_case__39 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_78_case__39(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_78_case__39 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_78_case__39")(x)



c__case_79_case__40 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x5)(st)))(st)
c__case_79_case__40 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_78(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(x1)(st))(x6)(st))(st)
c__case_79_case__40 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_79_case__40(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_79_case__40 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_79_case__40")(x)



c__case_80_case__41 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x5)(st)))(st)
c__case_80_case__41 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_79(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(x1)(st))(x6)(st))(st)
c__case_80_case__41 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_80_case__41(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_80_case__41 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_80_case__41")(x)



c__case_81_case__42 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x5)(st)))(st)
c__case_81_case__42 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_80(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(x1)(st))(x6)(st))(st)
c__case_81_case__42 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_81_case__42(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_81_case__42 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_81_case__42")(x)



c__case_82_case__43 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x1)(st)))(st)
c__case_82_case__43 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_81(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(x1)(st))(x5)(st))(st)
c__case_82_case__43 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_82_case__43(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_82_case__43 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_82_case__43")(x)



c__case_83_case__44 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x1)(st)))(st)
c__case_83_case__44 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_82(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))(x1)(st))(x5)(st))(st)
c__case_83_case__44 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_83_case__44(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_83_case__44 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_83_case__44")(x)



c__case_84_case__45 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x1)(st)))(st)
c__case_84_case__45 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_83(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))(x1)(st))(x5)(st))(st)
c__case_84_case__45 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_84_case__45(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_84_case__45 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_84_case__45")(x)



c__case_85_case__46 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x1)(st)))(st)
c__case_85_case__46 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_84(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List))))(x1)(st))(x5)(st))(st)
c__case_85_case__46 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_85_case__46(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_85_case__46 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_85_case__46")(x)



c__case_86_case__47 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x3)(x1)(st)))(st)
c__case_86_case__47 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_85(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))(x1)(st))(x5)(st))(st)
c__case_86_case__47 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_86_case__47(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_86_case__47 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_86_case__47")(x)



c__case_70_case__48 x1 x4 x6 x7@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.OracleRead.c_readHex(x6)(x1)(st))(x8)(st))(Curry.Module.Prelude.List))(st)
c__case_70_case__48 x1 x4 x6 x7@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.OracleRead.c_readInt(x4)(x1)(st))(x9)(st))(Curry.Module.Prelude.List))(st)
c__case_70_case__48 x1 x4 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_70_case__48(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_70_case__48 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_70_case__48")(x)



c__case_71_case__49 x1 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_70(x4)(x5)(x6)(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('x'))(x1)(st))(x7)(st))(st)
c__case_71_case__49 x1 x4@Curry.Module.Prelude.List st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.OracleRead.c_readInt(x4)(x1)(st))(x8)(st))(Curry.Module.Prelude.List))(st)
c__case_71_case__49 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_71_case__49(x1)(x)(st))(i)(xs)(st)
c__case_71_case__49 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_71_case__49")(x)



c__case_69_case__50 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(x3)(x4))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))(x1)(st)))(st)
c__case_69_case__50 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_69_case__50 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_69_case__50(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_69_case__50 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_69_case__50")(x)



c__case_72_case__51 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_71(x4)(x1)(st))(st)
c__case_72_case__51 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_69(x3)(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x6)(st))(st)
c__case_72_case__51 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_72_case__51(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_72_case__51 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_72_case__51")(x)



c__case_73_case__52 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_73_case__52 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_72(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('#'))(x1)(st))(x5)(st))(st)
c__case_73_case__52 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_73_case__52(x1)(x)(st))(i)(xs)(st)
c__case_73_case__52 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_73_case__52")(x)



c__case_67_case__53 x1 x2 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_error(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('X'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))(x1)(st))(x6)(st))(x7)(st))(st)
c__case_67_case__53 x1 x2 x4 x5@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_return(Curry.Module.OraclePrelude.c_head(x4)(x1)(st))(x8)(st))(st)
c__case_67_case__53 x1 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_67_case__53(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_67_case__53 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_67_case__53")(x)



c__case_68_case__54 x1 x2 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_error(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('X'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))(x1)(st))(x6)(st))(x7)(st))(st)
c__case_68_case__54 x1 x2 x4 x5@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c__case_67(x2)(x4)(Curry.Module.OraclePrelude.op_47_61(Curry.Module.OraclePrelude.c_tail(x4)(x1)(st))(Curry.Module.Prelude.List)(x8)(st))(x9)(st))(st)
c__case_68_case__54 x1 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_68_case__54(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_68_case__54 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_68_case__54")(x)



c__case_65_case__55 x1 x3@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_65_case__55 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_65_case__55(x1)(x)(st))(i)(xs)(st)
c__case_65_case__55 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_65_case__55")(x)



c__case_60_case__56 x1 x3 x5 x11 x12 x14 x24@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(x5))(st)
c__case_60_case__56 x1 x3 x5 x11 x12 x14 x24@Curry.Module.Prelude.C_False st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(let {x21 = Curry.Module.OracleXML.c_parseXmlTokens(x5)(x3)(x1)(st)} in let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x27)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem(x14)(x11)(x12))(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP17'35xexps(x21)(x25)(st)))(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP18'35rem_xtokens(x21)(x26)(st)))(st))(st))(st))(st)
c__case_60_case__56 x1 x3 x5 x11 x12 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_60_case__56(x1)(x3)(x5)(x11)(x12)(x14)(x)(st))(i)(xs)(st)
c__case_60_case__56 x1 x3 x5 x11 x12 x14 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_60_case__56")(x)



c__case_59_case__57 x1 x3 x5 x11 x12 x13 x14 x27@Curry.Module.Prelude.C_True st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(let {x24 = Curry.Module.OracleXML.c_parseXmlTokens(x5)(x3)(x1)(st)} in let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))(let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x30)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(x13)(x14))(x11)(x12))(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP20'35xexps(x24)(x28)(st)))(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP21'35rem_xtokens(x24)(x29)(st)))(st))(st))(st))(st)
c__case_59_case__57 x1 x3 x5 x11 x12 x13 x14 x27@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_59_case__57 x1 x3 x5 x11 x12 x13 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_59_case__57(x1)(x3)(x5)(x11)(x12)(x13)(x14)(x)(st))(i)(xs)(st)
c__case_59_case__57 x1 x3 x5 x11 x12 x13 x14 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_59_case__57")(x)



c__case_61_case__58 x1 x3 x5 x11 x12 x13 x14 x15@Curry.Module.Prelude.C_True st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c__case_60(x3)(x5)(x11)(x12)(x14)(Curry.Module.OraclePrelude.c_maybe(Curry.Module.Prelude.C_False)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_61_61))(st))(Curry.Module.OraclePrelude.c_tail(x14)(x1)(st))))))(x3)(x16)(st))(x17)(st))(st)
c__case_61_case__58 x1 x3 x5 x11 x12 x13 x14 x15@Curry.Module.Prelude.C_False st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_59(x3)(x5)(x11)(x12)(x13)(x14)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x18)(st))(st)
c__case_61_case__58 x1 x3 x5 x11 x12 x13 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_61_case__58(x1)(x3)(x5)(x11)(x12)(x13)(x14)(x)(st))(i)(xs)(st)
c__case_61_case__58 x1 x3 x5 x11 x12 x13 x14 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_61_case__58")(x)



c__case_62_case__59 x1 x3 x5 x11 x12 x13 x14 x21@Curry.Module.Prelude.C_True st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.OracleXML.c_parseXmlTokens(x5)(Curry.Module.Prelude.C_Just(x14))(x1)(st)} in let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.OracleXML.c_parseXmlTokens(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP15'35xtokens1(x15)(x23)(st))(x3)(x24)(st)} in let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x27)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem(x14)(x11)(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP14'35xexps1(x15)(x22)(st)))(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP12'35xexps(x18)(x25)(st)))(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP13'35rem_xtokens(x18)(x26)(st)))(st))(st))(st))(st))(st))(st))(st)
c__case_62_case__59 x1 x3 x5 x11 x12 x13 x14 x21@Curry.Module.Prelude.C_False st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List)))))(Curry.Module.OracleXML.c__case_61(x3)(x5)(x11)(x12)(x13)(x14)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x13)(Curry.Module.Prelude.C_Char('<'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_head(x14)(x28)(st))(Curry.Module.Prelude.C_Char('/'))(x29)(st))(x30)(st))(x31)(st))(st)
c__case_62_case__59 x1 x3 x5 x11 x12 x13 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_62_case__59(x1)(x3)(x5)(x11)(x12)(x13)(x14)(x)(st))(i)(xs)(st)
c__case_62_case__59 x1 x3 x5 x11 x12 x13 x14 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_62_case__59")(x)



c__case_63_case__60 x1 x3 x5 x11 x12 x10@((Curry.Module.Prelude.:<) x13 x14) st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))))(Curry.Module.OracleXML.c__case_62(x3)(x5)(x11)(x12)(x13)(x14)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x13)(Curry.Module.Prelude.C_Char('<'))(x1)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.OraclePrelude.c_head(x14)(x15)(st))(Curry.Module.Prelude.C_Char('/'))(x16)(st))(x17)(st))(x18)(st))(st)
c__case_63_case__60 x1 x3 x5 x11 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_63_case__60(x1)(x3)(x5)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_63_case__60 x1 x3 x5 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_63_case__60")(x)



c__case_64_case__61 x1 x3 x5 x4@(Curry.Module.XML.C_XText x6) st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.OracleXML.c_parseXmlTokens(x5)(x3)(x1)(st)} in let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x15)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XText(Curry.Module.OracleXML.c_xmlUnquoteSpecials(x6)(x15)(st)))(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP8'35xexps(x7)(x13)(st)))(Curry.Module.OracleXML.c_parseXmlTokens'46_'35selFP9'35rem_xtokens(x7)(x14)(st)))(st))(st))(st))(st)
c__case_64_case__61 x1 x3 x5 x4@(Curry.Module.XML.C_XElem x10 x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_63(x3)(x5)(x11)(x12)(x10)(x1)(st))(st)
c__case_64_case__61 x1 x3 x5 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_64_case__61(x1)(x3)(x5)(x)(st))(i)(xs)(st)
c__case_64_case__61 x1 x3 x5 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_64_case__61")(x)



c__case_66_case__62 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_65(x3)(x1)(st))(st)
c__case_66_case__62 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c__case_64(x3)(x5)(x4)(x1)(st))(st)
c__case_66_case__62 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_66_case__62(x1)(x3)(x)(st))(i)(xs)(st)
c__case_66_case__62 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_66_case__62")(x)



c__case_58_case__63 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_58_case__63 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_58_case__63(x1)(x)(st))(i)(xs)(st)
c__case_58_case__63 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_58_case__63")(x)



c__case_57_case__64 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_57_case__64 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_57_case__64(x1)(x)(st))(i)(xs)(st)
c__case_57_case__64 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_57_case__64")(x)



c__case_56_case__65 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_56_case__65 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_56_case__65(x1)(x)(st))(i)(xs)(st)
c__case_56_case__65 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_56_case__65")(x)



c__case_55_case__66 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_55_case__66 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_55_case__66(x1)(x)(st))(i)(xs)(st)
c__case_55_case__66 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_55_case__66")(x)



c__case_54_case__67 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_54_case__67 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_54_case__67(x1)(x)(st))(i)(xs)(st)
c__case_54_case__67 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_54_case__67")(x)



c__case_53_case__68 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_53_case__68 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_53_case__68(x1)(x)(st))(i)(xs)(st)
c__case_53_case__68 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_53_case__68")(x)



c__case_52_case__69 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_52_case__69 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_52_case__69(x1)(x)(st))(i)(xs)(st)
c__case_52_case__69 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_52_case__69")(x)



c__case_51_case__70 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_51_case__70 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_51_case__70(x1)(x)(st))(i)(xs)(st)
c__case_51_case__70 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_51_case__70")(x)



c__case_50_case__71 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_50_case__71 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_50_case__71(x1)(x)(st))(i)(xs)(st)
c__case_50_case__71 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_50_case__71")(x)



c__case_49_case__72 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_49_case__72 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_49_case__72(x1)(x)(st))(i)(xs)(st)
c__case_49_case__72 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_49_case__72")(x)



c__case_47_case__73 x1 x3 x4 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_scanXmlElem(x4)(x1)(st))(st)
c__case_47_case__73 x1 x3 x4 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleXML.c_scanXmlText((Curry.Module.Prelude.:<)(x3)(x4))(x1)(st)} in let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x11)((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XText(Curry.Module.OracleXML.c_scanXmlString'46scanXml'46191'46_'35selFP23'35initxt(x5)(x9)(st)))(Curry.Module.OracleXML.c_scanXmlString'46scanXml'46191(Curry.Module.OracleXML.c_scanXmlString'46scanXml'46191'46_'35selFP24'35remtag(x5)(x10)(st))(x11)(st)))(st))(st))(st))(st)
c__case_47_case__73 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_47_case__73(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_47_case__73 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_47_case__73")(x)



c__case_48_case__74 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_48_case__74 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_47(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('<'))(x1)(st))(x5)(st))(st)
c__case_48_case__74 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_48_case__74(x1)(x)(st))(i)(xs)(st)
c__case_48_case__74 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_48_case__74")(x)



c__case_46_case__75 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_46_case__75 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_46_case__75(x1)(x)(st))(i)(xs)(st)
c__case_46_case__75 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_46_case__75")(x)



c__case_45_case__76 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_45_case__76 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_45_case__76(x1)(x)(st))(i)(xs)(st)
c__case_45_case__76 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_45_case__76")(x)



c__case_41_case__77 x1 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_41_case__77 x1 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(x6))(st)
c__case_41_case__77 x1 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_41_case__77(x1)(x6)(x)(st))(i)(xs)(st)
c__case_41_case__77 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_41_case__77")(x)



c__case_40_case__78 x1 x3 x4 x11@Curry.Module.Prelude.C_True st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x8 = Curry.Module.OracleXML.c_scanXmlText(x4)(x1)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x14)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(Curry.Module.OracleXML.c_scanXmlText'46_'35selFP29'35txt(x8)(x12)(st)))(Curry.Module.OracleXML.c_scanXmlText'46_'35selFP30'35rem(x8)(x13)(st)))(st))(st))(st))(st)
c__case_40_case__78 x1 x3 x4 x11@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_40_case__78 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_40_case__78(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_40_case__78 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_40_case__78")(x)



c__case_42_case__79 x1 x3 x4 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))(let {x5 = Curry.Module.OracleXML.c_scanXmlText(Curry.Module.Oracle.c_apply(Curry.Module.OracleXML.c_dropBlanks(x1)(st))(x4)(x9)(st))(x10)(st)} in let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OracleXML.c_scanXmlText'46_'35selFP26'35txt(x5)(x11)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.Prelude.T2(Curry.Module.OracleXML.c__case_41(x6)(Curry.Module.OraclePrelude.c_null(x6)(x13)(st))(x14)(st))(Curry.Module.OracleXML.c_scanXmlText'46_'35selFP27'35rem(x5)(x12)(st)))(st))(st))(st))(st)
c__case_42_case__79 x1 x3 x4 x8@Curry.Module.Prelude.C_False st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_40(x3)(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x15)(st))(st)
c__case_42_case__79 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_42_case__79(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_42_case__79 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_42_case__79")(x)



c__case_43_case__80 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(x3)(x4)))(st)
c__case_43_case__80 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_42(x3)(x4)(Curry.Module.OracleChar.c_isSpace(x3)(x1)(st))(x6)(st))(st)
c__case_43_case__80 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_43_case__80(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_43_case__80 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_43_case__80")(x)



c__case_44_case__81 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_44_case__81 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_43(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('<'))(x1)(st))(x5)(st))(st)
c__case_44_case__81 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_44_case__81(x1)(x)(st))(i)(xs)(st)
c__case_44_case__81 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_44_case__81")(x)



c__case_39_case__82 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_39_case__82 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_39_case__82(x1)(x)(st))(i)(xs)(st)
c__case_39_case__82 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_39_case__82")(x)



c__case_38_case__83 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_38_case__83 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_38_case__83(x1)(x)(st))(i)(xs)(st)
c__case_38_case__83 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_38_case__83")(x)



c__case_37_case__84 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_37_case__84 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_37_case__84(x1)(x)(st))(i)(xs)(st)
c__case_37_case__84 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_37_case__84")(x)



c__case_36_case__85 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_36_case__85 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_36_case__85(x1)(x)(st))(i)(xs)(st)
c__case_36_case__85 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_36_case__85")(x)



c__case_33_case__86 x1 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_scanXmlComment(Curry.Module.OraclePrelude.c_drop(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x4)(x1)(st))(x6)(st))(st)
c__case_33_case__86 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_scanXmlCData(x4)(x1)(st))(st)
c__case_33_case__86 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_33_case__86(x1)(x4)(x)(st))(i)(xs)(st)
c__case_33_case__86 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_33_case__86")(x)



c__case_31_case__87 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_scanXmlElemName((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(x4)(x1)(st))(st)
c__case_31_case__87 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_31_case__87 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_31_case__87(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_31_case__87 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_31_case__87")(x)



c__case_32_case__88 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_scanXmlProcInstr(x4)(x1)(st))(st)
c__case_32_case__88 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_31(x3)(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x6)(st))(st)
c__case_32_case__88 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_32_case__88(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_32_case__88 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_32_case__88")(x)



c__case_34_case__89 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c__case_33(x4)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_take(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x4)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))(Curry.Module.Prelude.List)))(x6)(st))(x7)(st))(st)
c__case_34_case__89 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_32(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('?'))(x1)(st))(x8)(st))(st)
c__case_34_case__89 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_34_case__89(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_34_case__89 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_34_case__89")(x)



c__case_35_case__90 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_35_case__90 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_34(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('!'))(x1)(st))(x5)(st))(st)
c__case_35_case__90 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_35_case__90(x1)(x)(st))(i)(xs)(st)
c__case_35_case__90 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_35_case__90")(x)



c__case_27_case__91 x1 x2 x7 x8 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem(x2)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_scanXmlString(Curry.Module.OraclePrelude.c_drop(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x8)(x1)(st))(x10)(st)))(st)
c__case_27_case__91 x1 x2 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(x2))(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_scanXmlString(Curry.Module.OraclePrelude.c_tail(x8)(x1)(st))(x11)(st)))(st)
c__case_27_case__91 x1 x2 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_27_case__91(x1)(x2)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_27_case__91 x1 x2 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_27_case__91")(x)



c__case_25_case__92 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_scanXmlElemName(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(x1)(st))(x5)(x7)(st))(st)
c__case_25_case__92 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_25_case__92 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_25_case__92(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_25_case__92 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_25_case__92")(x)



c__case_26_case__93 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem(x2)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_scanXmlString(Curry.Module.OraclePrelude.c_tail(x5)(x1)(st))(x7)(st)))(st)
c__case_26_case__93 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_25(x2)(x4)(x5)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x8)(st))(st)
c__case_26_case__93 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_26_case__93(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_26_case__93 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_26_case__93")(x)



c__case_28_case__94 x1 x2 x4 x5 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))(let {x6 = Curry.Module.OracleXML.c_parseAttrs(Curry.Module.Oracle.c_apply(Curry.Module.OracleXML.c_dropBlanks(x1)(st))(x5)(x10)(st))(x11)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x8 = Curry.Module.OracleXML.c_scanXmlElemName'46_'35selFP33'35rest(x6)(x13)(st)} in let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c__case_27(x2)(Curry.Module.OracleXML.c_scanXmlElemName'46_'35selFP32'35attrs(x6)(x12)(st))(x8)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_head(x8)(x14)(st))(Curry.Module.Prelude.C_Char('/'))(x15)(st))(x16)(st))(st))(st))(st))(st)
c__case_28_case__94 x1 x2 x4 x5 x9@Curry.Module.Prelude.C_False st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))))(Curry.Module.OracleXML.c__case_26(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('/'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_head(x5)(x17)(st))(Curry.Module.Prelude.C_Char('>'))(x18)(st))(x19)(st))(x20)(st))(st)
c__case_28_case__94 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_28_case__94(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_28_case__94 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_28_case__94")(x)



c__case_29_case__95 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(x2))(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_scanXmlString(x5)(x1)(st)))(st)
c__case_29_case__95 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_28(x2)(x4)(x5)(Curry.Module.OracleChar.c_isSpace(x4)(x1)(st))(x7)(st))(st)
c__case_29_case__95 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_29_case__95(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_29_case__95 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_29_case__95")(x)



c__case_30_case__96 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(x2))(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(Curry.Module.Prelude.List))(st)
c__case_30_case__96 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_29(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('>'))(x1)(st))(x6)(st))(st)
c__case_30_case__96 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_30_case__96(x1)(x2)(x)(st))(i)(xs)(st)
c__case_30_case__96 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_30_case__96")(x)



c__case_24_case__97 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_24_case__97 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_24_case__97(x1)(x)(st))(i)(xs)(st)
c__case_24_case__97 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_24_case__97")(x)



c__case_23_case__98 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_23_case__98 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_23_case__98(x1)(x)(st))(i)(xs)(st)
c__case_23_case__98 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_23_case__98")(x)



c__case_21_case__99 x1 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_scanXmlString(Curry.Module.OraclePrelude.c_drop(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x4)(x1)(st))(x6)(st))(st)
c__case_21_case__99 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_scanXmlComment(x4)(x1)(st))(st)
c__case_21_case__99 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_21_case__99(x1)(x4)(x)(st))(i)(xs)(st)
c__case_21_case__99 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_21_case__99")(x)



c__case_22_case__100 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_22_case__100 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.OracleXML.c__case_21(x3)(x4)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('-'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_take(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x4)(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List)))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_22_case__100 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_22_case__100(x1)(x)(st))(i)(xs)(st)
c__case_22_case__100 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_22_case__100")(x)



c__case_20_case__101 x1 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_scanXmlString(Curry.Module.OraclePrelude.c_tail(x3)(x1)(st))(x5)(st))(st)
c__case_20_case__101 x1 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_scanXmlCData(x3)(x1)(st))(st)
c__case_20_case__101 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_20_case__101(x1)(x3)(x)(st))(i)(xs)(st)
c__case_20_case__101 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_20_case__101")(x)



c__case_16_case__102 x1 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_dropCData(x4)(x1)(st))(st)
c__case_16_case__102 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_16_case__102 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_16_case__102(x1)(x4)(x)(st))(i)(xs)(st)
c__case_16_case__102 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_16_case__102")(x)



c__case_17_case__103 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x3)(x4))(st)
c__case_17_case__103 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_16(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x6)(st))(st)
c__case_17_case__103 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_17_case__103(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_17_case__103 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_17_case__103")(x)



c__case_18_case__104 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_tail(Curry.Module.OraclePrelude.c_dropWhile(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(Curry.Module.Prelude.C_Char(']'))))))(x4)(x1)(st))(x6)(st))(st)
c__case_18_case__104 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_17(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('>'))(x1)(st))(x7)(st))(st)
c__case_18_case__104 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_18_case__104(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_18_case__104 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_18_case__104")(x)



c__case_19_case__105 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_19_case__105 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_18(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('['))(x1)(st))(x5)(st))(st)
c__case_19_case__105 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_19_case__105(x1)(x)(st))(i)(xs)(st)
c__case_19_case__105 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_19_case__105")(x)



c__case_14_case__106 x1 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_scanXmlString(Curry.Module.OraclePrelude.c_tail(x4)(x1)(st))(x6)(st))(st)
c__case_14_case__106 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_scanXmlProcInstr(x4)(x1)(st))(st)
c__case_14_case__106 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_14_case__106(x1)(x4)(x)(st))(i)(xs)(st)
c__case_14_case__106 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_14_case__106")(x)



c__case_15_case__107 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_15_case__107 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.OracleXML.c__case_14(x3)(x4)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('?'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_head(x4)(x5)(st))(Curry.Module.Prelude.C_Char('>'))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_15_case__107 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_15_case__107(x1)(x)(st))(i)(xs)(st)
c__case_15_case__107 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_15_case__107")(x)



c__case_11_case__108 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(x3)(x4)))(st)
c__case_11_case__108 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_11_case__108 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_11_case__108(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_11_case__108 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_11_case__108")(x)



c__case_12_case__109 x1 x3 x4 x14@Curry.Module.Prelude.C_True st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleXML.c_splitAtChar(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(x3)(x4))(x1)(st)} in let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))(let {x8 = Curry.Module.OracleXML.c_splitAtChar(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.OraclePrelude.c_tail(Curry.Module.OracleXML.c_parseAttrs'46_'35selFP42'35rest1(x5)(x16)(st))(x17)(st))(x18)(st)} in let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))))(let {x11 = Curry.Module.OracleXML.c_parseAttrs(Curry.Module.Oracle.c_apply(Curry.Module.OracleXML.c_dropBlanks(x21)(st))(Curry.Module.OracleXML.c_parseAttrs'46_'35selFP40'35rest2(x8)(x20)(st))(x22)(st))(x23)(st)} in let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x26)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.OracleXML.c_parseAttrs'46_'35selFP41'35name(x5)(x15)(st))(Curry.Module.OracleXML.c_xmlUnquoteSpecials(Curry.Module.OracleXML.c_parseAttrs'46_'35selFP39'35value(x8)(x19)(st))(x26)(st)))(Curry.Module.OracleXML.c_parseAttrs'46_'35selFP37'35rem_attrs(x11)(x24)(st)))(Curry.Module.OracleXML.c_parseAttrs'46_'35selFP38'35rem_inp(x11)(x25)(st)))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_12_case__109 x1 x3 x4 x14@Curry.Module.Prelude.C_False st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_11(x3)(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x27)(st))(st)
c__case_12_case__109 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_12_case__109(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_12_case__109 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_12_case__109")(x)



c__case_13_case__110 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_13_case__110 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_12(x3)(x4)(Curry.Module.OracleChar.c_isAlpha(x3)(x1)(st))(x5)(st))(st)
c__case_13_case__110 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_13_case__110(x1)(x)(st))(i)(xs)(st)
c__case_13_case__110 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_13_case__110")(x)



c__case_10_case__111 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_10_case__111 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_10_case__111(x1)(x)(st))(i)(xs)(st)
c__case_10_case__111 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_10_case__111")(x)



c__case_9_case__112 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_9_case__112 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_9_case__112(x1)(x)(st))(i)(xs)(st)
c__case_9_case__112 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_9_case__112")(x)



c__case_8_case__113 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_8_case__113 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_8_case__113(x1)(x)(st))(i)(xs)(st)
c__case_8_case__113 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_8_case__113")(x)



c__case_7_case__114 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_7_case__114 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_7_case__114(x1)(x)(st))(i)(xs)(st)
c__case_7_case__114 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_7_case__114")(x)



c__case_6_case__115 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_6_case__115 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_6_case__115(x1)(x)(st))(i)(xs)(st)
c__case_6_case__115 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_6_case__115")(x)



c__case_5_case__116 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_5_case__116 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_5_case__116(x1)(x)(st))(i)(xs)(st)
c__case_5_case__116 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_5_case__116")(x)



c__case_3_case__117 x1 x2 x4 x5 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(x5))(st)
c__case_3_case__117 x1 x2 x4 x5 x9@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OracleXML.c_splitAtChar(x2)(x5)(x1)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x12)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x4)(Curry.Module.OracleXML.c_splitAtChar'46_'35selFP44'35first(x6)(x10)(st)))(Curry.Module.OracleXML.c_splitAtChar'46_'35selFP45'35rest(x6)(x11)(st)))(st))(st))(st))(st)
c__case_3_case__117 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_3_case__117(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_3_case__117 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_3_case__117")(x)



c__case_4_case__118 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_4_case__118 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c__case_3(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x4)(x2)(x1)(st))(x6)(st))(st)
c__case_4_case__118 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_4_case__118(x1)(x2)(x)(st))(i)(xs)(st)
c__case_4_case__118 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_4_case__118")(x)



c__case_2_case__119 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_2_case__119 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_2_case__119(x1)(x)(st))(i)(xs)(st)
c__case_2_case__119 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_2_case__119")(x)



c__case_1_case__120 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_1_case__120 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_1_case__120(x1)(x)(st))(i)(xs)(st)
c__case_1_case__120 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_1_case__120")(x)



c__case_0_case__121 x1 x2@(Curry.Module.XML.C_XText x3) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_0_case__121 x1 x2@(Curry.Module.XML.C_XElem x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_textOfXml(x6)(x1)(st))(st)
c__case_0_case__121 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleXML.c__case_0_case__121(x1)(x)(st))(i)(xs)(st)
c__case_0_case__121 x1 x st = Curry.RunTimeSystem.patternFail("OracleXML._case_0_case__121")(x)


