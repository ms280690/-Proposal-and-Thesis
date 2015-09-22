{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}


module Experiment(float
	)where

-- Mehul Solanki.

import Control.Monad
import Text.Parsec
import Control.Applicative hiding ((<|>))
import Data.Functor.Identity
import Text.Parsec.Prim

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

--number :: Stream s m Char => ParsecT s u m [Char]
number :: ParsecT [Char] u Data.Functor.Identity.Identity [Char]
number = many1 digit

--plus :: Stream s m Char => ParsecT s u m [Char]
plus :: ParsecT [Char] u Data.Functor.Identity.Identity [Char]
plus = char '+' *> number

--minus :: Stream s m Char => ParsecT s u m [Char]
minus :: ParsecT [Char] u Data.Functor.Identity.Identity [Char]
minus = char '-' <:> number

--integer :: Stream s m Char => ParsecT s u m [Char]
integer :: ParsecT [Char] u Data.Functor.Identity.Identity [Char]
integer = plus <|> minus <|> number

--float :: Stream s m Char => ParsecT s u m Float
--float :: ParsecT [Char] u Data.Functor.Identity.Identity Float
--float =  fmap rd $ integer <++> decimal <++> exponent
--    where rd       = read :: String -> Float
--          decimal  = option "" $ char '.' <:> number
--          exponent = option "" $ oneOf "eE" <:> integer

--float :: ParsecT [Char] u Identity [Char]
float = integer <++> decimal <++> exponent
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer

main :: IO ()          
main = forever $ do putStrLn "Enter a float: "
                    input <- getLine
                    parseTest float input

