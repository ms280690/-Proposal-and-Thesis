{-# Language FlexibleContexts #-}

module QuoteParser
   {-
   ( consult, consultString, parseQuery
   , program, whitespace, comment, clause, terms, term, vname
   )
   -}
   where

{-
    This module is designed to be an interface between the generic
    parser defined in Parser.hs and the quasi-quotation module defined
    in Quote.hs.  In particular, operators are read with their standard
    bindings.
-}

import Control.Applicative ((<$>),(<*>))
import Data.Generics (everywhere, mkT)
import Text.Parsec

import Syntax
import qualified Parser
import Unflatten(defaultDict,unflatten)
import qualified Data.Data as Data

{-========================================================================-}
  -- top-level grammar parsers
{-========================================================================-}

fixOps :: Data.Data a => a -> a
fixOps x = everywhere (mkT fixTerm) x
fixTerm :: Term -> Term
fixTerm xs = unflatten defaultDict xs


program ::  Stream s m Char => ParsecT s u m Program
program = fixOps <$> Parser.program

clause ::  Stream s m Char => ParsecT s u m Clause
clause = fixOps <$> Parser.clause

terms ::  Stream s m Char => ParsecT s u m [Term]
terms = sepBy1 QuoteParser.term (Parser.charWs ',')

term :: Stream s m Char => ParsecT s u m Term
term = fixOps <$> Parser.term

{-========================================================================-}
  -- Tokens and their subcomponents
{-========================================================================-}

whitespace ::  Stream s m Char => ParsecT s u m ()
whitespace = Parser.whitespace

