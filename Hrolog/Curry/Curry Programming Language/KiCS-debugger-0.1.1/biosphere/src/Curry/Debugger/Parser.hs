module Curry.Debugger.Parser where

-- This module should be deleted and referencing imports replaced by imports of the parser combinators included in the haskell library

infixl 4 <$>, <*, *>, <*>
infixl 3 <||>

type Parser a = String -> Pos -> Result a

data Result a = Result a String Pos
              | Error String
   deriving Show


type Pos = (Int,Int)

row :: Pos -> Int
row (_,x) = x

col :: Pos -> Int
col (x,_) = x

incPos :: Char -> Pos -> Pos
incPos '\n' (_,y) = (0,y+1)
incPos _    (x,y) = (x+1,y)

showPos :: Pos -> String
showPos (x,y) = "at line " ++ show x ++ " at column " ++ show y


pSucceed :: a -> Parser a
pSucceed = Result

pFail :: Parser a
pFail _ pos = Error ("Fail " ++ showPos pos)

pSym :: Char -> Parser Char
pSym c = pPred (c==) 

pPred :: (Char -> Bool) -> Parser Char
pPred p [] _       = Error "End of File"
pPred p (t:ts) pos | p t       = Result t ts (incPos t pos)
                   | otherwise = err
 where
  err = Error ("unexpected Symbol " ++ show t ++ " " ++ showPos pos)

(<||>) :: Parser a -> Parser a -> Parser a
(p <||> q) input pos = 
  case p input pos of
       Error _ -> q input pos
       result  -> result

(<*>) :: Parser (b -> a) -> Parser b -> Parser a
(p <*> q) input pos1 = 
  case p input pos1 of
       Result pv qinput pos2 ->
              case q qinput pos2 of
                   Result qv rest pos3 -> Result (pv qv) rest pos3 
                   Error err -> Error err
       Error err -> Error err
                  
(<->>) :: Parser a -> (a -> Parser b) -> Parser b
(p <->> qf) ts pos1 = 
  case p ts pos1 of
       Result pv ts1 pos2 ->
              case qf pv ts1 pos2 of
                   Error err -> Error err
                   result -> result
       Error err -> Error err


(<$>) :: (b -> a) -> Parser b -> Parser a
f <$> p = pSucceed f <*> p

f <$ p = const f <$> p

(<*) :: Parser a -> Parser b -> Parser a
p <* q = (\ x _ -> x) <$> p <*> q

(*>) :: Parser a -> Parser b -> Parser b
p *> q = (\ _ x -> x) <$> p <*> q

