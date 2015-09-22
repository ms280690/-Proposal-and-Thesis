{-
  Since version 0.7 of the language report, Curry accepts literate
  source programs. In a literate source all program lines must begin
  with a greater sign in the first column. All other lines are assumed
  to be documentation. In order to avoid some common errors with
  literate programs, Curry requires at least one program line to be
  present in the file. In addition, every block of program code must be
  preceded by a blank line and followed by a blank line.

  This module has been rewritten by Holger Siegel in 2009.

  (c) Holger Siegel, 2009.
-}


module Curry.Syntax.Unlit(unlit) where

import Control.Monad(when, zipWithM)
import Data.Char

import Curry.Base.Position
import Curry.Base.MessageMonad


data Line = Program !Int String
          | Blank | Comment

classify :: Int -> String -> Line
classify l ('>':cs)  = Program l cs
classify _ cs
  | all isSpace cs = Blank
  | otherwise      = Comment

{-
  Process a literate program into error messages (if any) and the
  corresponding non-literate program.
-}

unlit :: FilePath -> String -> MsgMonad String
unlit fn lcy = do ls <- progLines fn (zipWith classify [1..] $ lines lcy)
                  when (all null ls) $
                       failWith (fn ++ ": no code in literate script")
                  return (unlines ls)

{-
  Check that each program line is not adjacent to a comment line and
  there is at least one program line.
-}
progLines :: FilePath -> [Line] -> MsgMonad [String]
progLines fn cs 
   = zipWithM adjacent (Blank : cs) cs
  where
    adjacent :: Line -> Line -> MsgMonad String
    adjacent (Program p _) Comment     = message fn p "followed"
    adjacent Comment     (Program p _) = message fn p "preceded"
    adjacent _ (Program _ s)           = return s
    adjacent _ _                       = return ""

message :: String -> Int -> String -> MsgMonad a
message file p w = failWithAt (Position file p 1 noRef) msg
    where msg = "When reading literate source: Program line is " ++ w ++ " by comment line."
