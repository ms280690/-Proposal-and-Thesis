

-- Mehul Solanki.

-- Playing with different parsec Libraries to parse signed flaoting numbers.



-- Adjustable Number Parsers modified from Parsec.Token
import Text.ParserCombinators.Parsec.Number as Numbers

-- Primitive Parser Combinators
import Text.ParserCombinators.Parsec.Prim as Primitive

import Control.Monad

--import Numeric

--import Control.Applicative

--import Text.ParserCombinators.Parsec.Combinator

--import Text.ParserCombinators.Parsec.Char 

--import Text.Parsec.Numbers as Tools

floatParser :: [Char] -> IO ()
floatParser x = Primitive.parseTest (ap sign (Numbers.floating3 True)) x 

{--
So there are some functions that are provided by the libraires that actually do all the work that you want to do with a number of functions 
please refer to Experiment.hs for that. 

floating take a Bool
True ==> 3. == Error
False ==> 3. == Acceptable == 3.0

.3 ==> 0.3

but the sign thing is not working I have to look into that
--}


intParser x = Primitive.parseTest (Numbers.int) x

{--


--}



{--
p_number :: CharParser () Double
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty

--}


