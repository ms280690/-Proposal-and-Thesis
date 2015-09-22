% ShowTerm.lhs
% Andreas Baldeau, Christoph Wulf
% April 13, 2009

Overview
========

> module Curry.Debugger.ShowTerm (
>     showTerm,
>     showGenTerm,
>     printTerm,

	>     showsTerm,
	>     showsAlgebraicTerm,
	>     calcNat,
	>     showsRegularTerm

> ) where

Imports
-------

> import Prelude as P
> import Data.Map as Map
> import Control.Monad.Trans

> import Curry.Debugger.DebugInfo
> import Curry.Debugger.Logic
> import Curry.Debugger.Bubble
> import Curry.Debugger.Parser
> import List

Interface
=========

Shows given term.

> showTerm :: Term -> String
> showTerm t = showsTerm False 
>                (maybe (TermFail undefined) id $  value $ bubble t) ""

Converts given data to term and shows it.

> showGenTerm :: GenTerm a => a -> String
> showGenTerm = showTerm . genTerm

Converts given data to term and prints it to lifted IO of current monad.

> printTerm :: (GenTerm a,MonadIO m) => a -> m ()
> printTerm x = liftIO (putStr (showGenTerm x))

Implementation
==============

> type PrecInfo = Bool

> showsTerm :: PrecInfo -> Term -> ShowS
> showsTerm b t@(Term _ _ _) = showsAlgebraicTerm b t
> showsTerm _ (TermUnderscore _) = showChar '_'
> showsTerm _ (TermFail _)       = showChar '!'
> showsTerm b (TermOr _ r [x,y]) = 
>   showParen b (showsTerm True x . 
>                showString " ? " . 
>                showsTerm True y)
> showsTerm b (TermOr si r xs)    = showsTerm b (Term "Or" si xs)
> showsTerm _ (TermChar c)  = showChar '\'' . showChar c . showChar '\''
> showsTerm b (TermInt i)   = showParen (b && i<0) (shows i)
> showsTerm b (TermFloat f) = showParen (b && f<0) (shows f)
> showsTerm b (TermExt _ v) = showParen True (showString "#Ext: " . showString v)

Shows terms of algebraic data, handles special constructors (numbers, lists) and delegates others to `showsRegularTerm`.

> showsAlgebraicTerm :: PrecInfo -> Term -> ShowS
> showsAlgebraicTerm b t@(Term s SrcID{modID="Prelude"} ts) = case s of
>   "Zero" -> showChar '0'
>   "Pos"  -> nat
>   "Neg"  -> showChar '-' . nat
>   "[]"   -> showString "[]"
>   ":"    -> showsListTerm b t
>   "O"    -> nat'
>   "I"    -> nat'
>   "IHi"  -> nat'

Parse other constructors of Prelude against tuples, else treat them as regular constructors:

>   _      -> tuple
>   where
>     tuple = case isTuple s of
>               Just n  -> showsTupleTerm b t n
>               Nothing -> showsRegularTerm b t
>     nat   = case calcNat (head ts) of
>               Just n  -> shows n
>               Nothing -> showsRegularTerm b t
>     nat'  = case calcNat t of
>               Just n  -> shows n
>               Nothing -> showsRegularTerm b t

All not Prelude concstructors are treated as regular constructors:

> showsAlgebraicTerm b t = showsRegularTerm b t

Converts a term of `Nat` to a haskell number.

> calcNat (Term s _ ts) = case s of
>   "O"   -> case mnat of
>              Just nat -> Just $ 2 * nat 
>              Nothing  -> Nothing
>   "I"   -> case mnat of
>              Just nat -> Just $ 2 * nat + 1
>              Nothing  -> Nothing
>   "IHi" -> Just 1
>   _     -> error $ "unknown cons of Nat: " ++ s
>   where
>     mnat = calcNat (head ts)
> calcNat _ = Nothing

Shows a list term. Checks wether it's a String.

> showsListTerm b t = case t of
>   (Term ":" _ [TermChar _,_]) -> showChar '"' . showsStringTerm t
>   _                           -> showChar '[' . showsList b t

Shows a string term (list term of char terms).

> showsStringTerm t = case t of
>   (Term ":"  _ [TermChar c,rt]) -> showChar c . showsStringTerm rt
>   (Term "[]" _ _)               -> showChar '"'

> showsList :: PrecInfo -> Term -> ShowS
> showsList b (Term ":"  _ [te,(Term "[]" _ _)]) = 
>   showsTerm b te . showChar ']'
> showsList b (Term ":"  _ [te,tl]) = 
>   showsTerm b te . showChar ',' . showsList b tl
> showsList _ (Term "[]" _ _)       = showChar ']'
> showsList _ (TermUnderscore _)    = showString "...]"
> showsList b t = showsTerm b t


> showsRegularTerm b (Term s _ []) = showString s
> showsRegularTerm b (Term s _ ts) = 
>   showParen b $ showString s .
>                 foldr (\t fs -> showChar ' ' . showsTerm True t . fs) id ts

Checks if constructor name is a tuple, returns the arity of the tuple or nothing:

> isTuple s =
>   case pTuple s (0,0) of
>     Result 0 "" _  -> Just 0
>     Result i "" _  -> Just (i+1)
>     _              -> Nothing

Tuple parser:

> pTuple :: Parser Int
> pTuple = pSucceed (\_ n -> n)
>          <*> pSym '(' <*> tupleInterior

> tupleInterior :: Parser Int
> tupleInterior = pSucceed (\_ n -> n+1)
>                  <*> pSym ',' <*> tupleInterior
>                 <||>
>                 pSucceed (const 0)
>                  <*> pSym ')'

Shows the tuple of given term:

> showsTupleTerm b (Term _ _ []) 0 = showString "()"
> showsTupleTerm b (Term _ _ ts) n = 
>   showParen b $ showChar '(' . showString interior . showChar ')'
>   where
>     interior = concat $ intersperse "," terms
>     terms    = P.map showTerm ts
