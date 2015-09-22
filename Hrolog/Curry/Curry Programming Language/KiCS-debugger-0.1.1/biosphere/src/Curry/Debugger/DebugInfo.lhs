> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}

% DebugInfo.lhs
% Andreas Baldeau
% April 13, 2009

Overview
========

> module Curry.Debugger.DebugInfo (
>     DI (..),
>     StaticInfo (..),
>     DynamicInfo (..),
>     ModID,
>     DefID,
>     Term (..),
>     GenTerm (..)
> ) where

> import Data.Generics
> import Curry.Debugger.Logic

The representation of debug information
=======================================

`DI`
----

Our debug information consist of two different kinds of information. The first
ones are static information i.e. information about the executed code.

> data DI = DebugInfo {
>               staticInfo :: StaticInfo,
>               dynamicInfo :: DynamicInfo
>           }
>  deriving Show

`StaticInfo`
------------

`SrcID` holds a reference to the position in code where the function / data
type / etc. of intrest is defined.

> data StaticInfo  = SrcID {
>                        modID :: ModID,
>                        defID :: DefID
>                    }
>                  | SrcDescr String -- ???
>  deriving (Show,Eq,Data,Typeable)

> type ModID = String -- for the moment its the file name
>            --Integer 
> type DefID = Integer

`DynamicInfo`
-------------

The second kind of information are dynamic ones. They hold concrete values as
arguments to a function call, local variables, etc.

> data DynamicInfo = DynamicInfo {
>                        context :: [(StaticInfo, Term)],
>                        currentValues :: [Term]
>                    }
>  deriving Show

`Term`
------

> data Term = Term String StaticInfo [Term]
>           | TermChar Char
>           | TermInt Integer -- deprecated
>           | TermFloat Float
>           | TermExt StaticInfo ExternalValue
>           | TermUnderscore StaticInfo
>           | TermOr StaticInfo OrRef [Term]
>           | TermFail StaticInfo
>  deriving (Show,Eq,Data,Typeable)

> type ExternalValue = String

The class `GenTerm`
===================

The static information is generated during the transformation of the modules
whereas dynamic information has to be generated at runtime. To be able to get
a represantion e.g. of the value returned by a function one can call `genTerm`
as there is an instance of `GenTerm` generated for every data type.

> class Data a => GenTerm a where
>   genTerm :: a -> Term

	`underscore` returns a representation of a not evaluated expression.

	>   underscore :: a


	The class `GenStaticInfo`
	=========================

	Provides a function to return the static information of a term for given constructor.

	> class GenStaticInfo a where
	>   genStaticInfo :: a -> StaticInfo


	The class `DebugType`
	=====================

	Class that has (is going to / GenTerm deprecated) to be available for all transformed types and provides functions for
	- generating terms for given constructors
	- returning a representation failed calculations
	- creating non-deterministic branches for the type
	- returning a representation of not evaluated terms

	> class DebugType a where
	>   generateTerm :: a -> Term
	>   genfailed :: a
	>   makeOr :: OrRef -> [a] -> a
	>   genunderscore :: a

	`DebugType` instance for `Data` and `GenStaticInfo` types
	---------------------------------------------------------

	Generic instance for all types providing an instance of `Data`.

	> instance (Data a,GenStaticInfo a) => DebugType a where

	>   generateTerm x = let si   = genStaticInfo x
	>                        cons = toConstr x in
	>     case constrIndex cons of

	  Special terms for special constructors:

	>       1 -> TermFail si 
	>       2 -> TermOr si (orRef x) (map generateTerm (branches x))
	>       3 -> TermUnderscore si

	  Regular term for a regular constructor:

	>       _ -> Term (showConstr cons) si [] -- (map generateTerm (args x))

	  The failed representation is the 1st constructor of a transformed type:

	>   genfailed = let res = fromConstr $ indexConstr (dataTypeOf res) 1 in res

	  The or branch representation is the 2nd constructor of a transformed type:

	>   makeOr ref xs = 
	>     let orWithUndefined  = fromConstr $ indexConstr (dataTypeOf (head xs)) 2
	>         orWithArgs = gmapT (castArgs ref xs) orWithUndefined
	>       in orWithArgs

	  The representation for not evaluated calculations is the 3rd constructor of a transformed type:

	>   genunderscore = let res = fromConstr $ indexConstr (dataTypeOf res) 3 in res
