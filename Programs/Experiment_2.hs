

-- Mehul Solanki.

-- Both the imports refer to the modules in the parsec3 packages. 

-- This module is from the parsec3-numbers-0.0.4. It provides facilities similar to those of the original parsec-numbers package adjusted to work with 
-- parsec3-1.0.0.7. This module has been imported to use the parser functions for floating poin numbers.
import Text.Parsec.Number as Numbers

-- This is the same case as of the above, this module is from the parsec3-1.0.0.7 package itself. This module has been imported to use the test functions for
-- parsers. 
import Text.Parsec.Prim as Primitive


-- This is an example from 
-- http://www.vex.net/~trebla/haskell/parsec-generally.xhtml

import Text.Parsec

import Control.Monad.IO.Class

play :: String -> IO (Either ParseError Double)
play s = Primitive.runParserT pmain () "parameter" s

pmain :: ParsecT [Char] () IO Double
pmain = do
  x <- read `fmap` many1 digit
  eof
  return x

--pnum = do
--  x <- read `fmap` many1 digit
--  liftIO (putStrLn "bling!")
--  return x

--pplus = char '+' >> return (+)
