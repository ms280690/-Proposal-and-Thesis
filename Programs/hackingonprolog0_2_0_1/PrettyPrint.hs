module PrettyPrint where

import Data.List(intercalate,intersperse)
import Data.Char (isLetter,chr,ord)

import Syntax

instance Show VariableName where
   show (VariableName 0 v) = v
   show (VariableName i v) = v ++ "#" ++  show i

instance Show Clause where
   show (Clause   lhs [] ) = show lhs
   show (Clause   lhs rhs) = show lhs ++ " :- " ++ intercalate ", " (map show rhs)
   show (ClauseFn lhs _  ) = show lhs ++ " :- " ++ "<Haskell function>"

instance Show Sentence where
  showList [] s = s
  showList (x:xs) s = showsPrec 0 x ("\n\n" ++ showList xs s)
  showsPrec = showSentencePrec

instance Show Term where
  showsPrec = showTermS

instance Show FlatItem where
  showsPrec = showsPrecFlatItemS

instance Show Atom where
  show = prettyPrintAtom

showSentencePrec d s = case s of
  Query body    -> showString "?- " . showsPrec d body . showChar '.'
  Command body  -> showString ":- " . showsPrec d body . showChar '.'
  C clause      -> showsPrec d clause                  . showChar '.'


showEmbracedItems :: (Show a) => String -> String  -> String -> [a] -> ShowS
showEmbracedItems left right comma list acc =
  left ++ foldr ($) (right++acc) 
  (intersperse (showString comma) . map (showsPrec 0) $ list)

showParenList :: (Show a) => [a] -> ShowS
showParenList = showEmbracedItems "(" ")" "," 

showTermS :: Int -> Term -> ShowS
showTermS d t = case t of
  Struct a []       -> showsPrec d a
  Struct a ts       -> showsPrec d a . showParenList ts
  Var v             -> showsPrec d v
  Wildcard          -> showString "_"
  PString   s       -> shows s
  PInteger  i       -> shows i
  PFloat    ff      -> shows ff
  Flat fItems       -> showEmbracedItems "<{< " " >}>" "," fItems
  Cut _             -> showString "!"

showsPrecFlatItemS :: Int -> FlatItem -> ShowS
showsPrecFlatItemS d x = case x of
  Bracket xs   -> showParen True $ showsPrec 0 xs
  FITerm t     -> showsPrec d t
  FIOperator o -> showsPrec d o

prettyPrintAtom (Operator x) = x
prettyPrintAtom (Atom x) =
  if any needsQuote x
  then '\'':concatMap quoteChar x ++ "'"
  else x
  where
    needsQuote c = n<32 || n>126 || c== '\'' || c== '\\' where n = ord c
    quoteChar c
      | c=='\''     = "''"
      | c=='\\'     = replicate 2 '\\'
      | c==chr 7    = escape 'a'
      | c==chr 8    = escape 'b'
      | c==chr 10   = escape 'n'
      | c==chr 11   = escape 'v'
      | c==chr 12   = escape 'f'
      | c==chr 13   = escape 'r'
      | c < ' '     = ['\\', '^', chr (ord '@' + ord c)]
      | c==chr 127  = escape 'd'
      | c> chr 127  = ['\\'
                      , chr (ord '0'+ord c `div` 64)
                      , chr (ord '0'+ (ord c `mod` 64) `div` 8)
                      , chr (ord '0'+ (ord c `mod` 8))
                      ]
      | otherwise   = [c]
      where escape c = ['\\',c]

