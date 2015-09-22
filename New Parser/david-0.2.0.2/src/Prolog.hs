{-# LANGUAGE  TemplateHaskell, QuasiQuotes,
              ViewPatterns,
              GeneralizedNewtypeDeriving,
              FlexibleInstances, FlexibleContexts,
              UndecidableInstances, IncoherentInstances #-}

module Prolog
   ( Term(..), var, cut
   , Clause(..), rhs
   , Sentence(..)
   , FlatItem(..)
   , VariableName(..), Atom(..), Unifier, Substitution, Program, Goal
   , unify, unify_with_occurs_check
   , apply
   , MonadTrace(..)
   , withTrace
   , MonadGraphGen(..)
   , runNoGraphT
   , resolve, resolve_
   , (+++)
   , consult, consultString, parseQuery
   , program, whitespace, comment, clause, terms, term, vname
   )
where

import Syntax
import Parser
import Unifier
import Interpreter

import Control.Monad
import Control.Monad.Error

my_Unify :: (Monad m) => Term -> Term -> m [Unifier]
my_Unify t1 t2 = do
   let x = unify t1 t2
   return x
{--
my_Unify (Var $ VariableName 0 "X") (PInteger 5)
[[(X,5)]]

my_Unify (Struct (Atom "hello") [Var $ VariableName 0 "X"]) (Struct (Atom "hello") [PInteger 5])
[[(X,5)]]
--}


my_Unify_with_occurs_check :: (Monad m) => Term -> Term -> m [Unifier]
my_Unify_with_occurs_check t1 t2 = do
   let x = unify_with_occurs_check t1 t2
   return x

{--
my_Unify (Var $ VariableName 0 "X") (Struct (Atom "hello") [Var $ VariableName 0 "X"])
[[(X,hello(X))]]

my_Unify_with_occurs_check  (Var $ VariableName 0 "X") (Struct (Atom "hello") [Var $ VariableName 0 "X"])
[]

--}


