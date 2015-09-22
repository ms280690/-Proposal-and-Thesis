  {-# LANGUAGE MagicHash, TemplateHaskell, FlexibleInstances #-}
module Quote (tm,tms,c,pl) where

import Control.Applicative ((<*))
import Data.Ratio((%))
import GHC.Exts(Double(..))
import Language.Haskell.TH (conP, varP,litP,
                            litE, Lit(..),listP, appE, conE,
                            listE, varE, viewP, mkName, Q, Exp, Pat)
import Language.Haskell.TH.Syntax (Lift(lift))
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Quote (QuasiQuoter(..),dataToPatQ)
import Text.Parsec (parse, eof,Parsec)
import Data.Generics (extQ, typeOf, Data)

import QuoteParser (term, terms, clause, program, whitespace)
import Syntax ( Term(..), VariableName(..), Clause(..), Goal
              , FlatItem, Atom(..), Sentence
              )


instance Lift Double where
  lift x =  appE  (conE 'D#) . litE . DoublePrimL . approx $ x where
    approx :: Double -> Rational
    approx x = let (m,n) = decodeFloat x in m % (floatRadix x)^(-n)


$(deriveLiftMany [''Sentence,
                  ''Atom, ''FlatItem, ''Term, ''VariableName, ''Clause])

instance Lift ([Term] -> [Goal]) where
   lift _ = fail "Clauses using Haskell functions can't be lifted."


tm,tms,c,pl :: QuasiQuoter
tm  = prologQuasiQuoter term    "term"
tms = prologQuasiQuoter terms   "term list"
c   = prologQuasiQuoter clause  "clause"
pl  = prologQuasiQuoter program "program"

type ParserName = String

prologQuasiQuoter :: (Data a, Lift a) =>
                     Parsec String () a -> ParserName -> QuasiQuoter
prologQuasiQuoter parser name =
   QuasiQuoter { quoteExp  = parsePrologExp parser name
               , quotePat  = parsePrologPat parser name
               , quoteType = fail ("Prolog "++ name ++"s can't be Haskell types!")
               , quoteDec  = fail ("Prolog "++ name ++"s can't be Haskell declarations!")
               }

parsePrologExp :: (Data a, Lift a) =>
                  Parsec String () a -> ParserName -> String -> Q Exp
parsePrologExp parser name str = do
   case parse (whitespace >> ((parser <* whitespace) <* eof)) ("(Prolog " ++ name ++ " expression)") str of
      Right x -> const (fail $ "Quasi-quoted expressions of type " ++ show (typeOf x) ++ " are not implemented.")
          `extQ` unquote                     -- Term
          `extQ` (listE . map unquote)       -- [Term]
          `extQ` unquoteClause               -- Clause
          `extQ` (listE . map unquoteClause) -- [Clause]
           $ x
      Left e -> fail (show e)
  where
   unquote (Struct (Operator "$") [Var (VariableName 0 varStr)])
     = [e| $(varE . mkName $ "pVar_"++varStr) |]
   unquote (Struct (Operator "$") _)
                        = fail "Found '$' with non-unquotable arguments"
   unquote (Struct a   xts) = [e| Struct a $(listE $ map unquote xts) |]
   unquote xt               = lift xt

   unquoteClause (Clause lhs rhs) =
      [e| Clause $(unquote lhs) $(listE $ map unquote rhs) |]
   unquoteClause (ClauseFn _ _) =
      fail "Clauses using Haskell functions are not quasi-quotable."


parsePrologPat :: (Data a, Lift a) =>
                  Parsec String () a -> ParserName -> String -> Q Pat
parsePrologPat parser name str = do
       case parse (whitespace >> parser <* eof)
                  ("(Prolog " ++ name ++ " pattern)") str of {
          Right x ->
            dataToPatQ 
            (const Nothing
              `extQ` (Just . unquote)                      -- Term
              `extQ` (Just . listP . map unquote)          -- [Term]
            )  $ x ;
          Left e  -> fail (show e) }
   where
     unquote :: Term -> Q Pat
     unquote (Var (VariableName _ vStr))
       = varP . mkName $ "pVar_" ++ vStr
     unquote Wildcard      = [p| _ |]
     unquote (Struct a xts) = unquoteStruct a xts
     unquote (PString  s)   = litP . StringL $ s
     unquote (PInteger i)   = litP . IntegerL $ i
     unquote q = viewP [e| (== $(lift q)) |] [p| True |]
     unquoteStruct a xts = do
       let functor = dataToPatQ (const Nothing) a
           args    =  map unquote xts
       conP 'Struct [functor,listP args]
