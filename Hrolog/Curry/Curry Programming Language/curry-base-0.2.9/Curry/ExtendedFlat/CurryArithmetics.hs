{-
  In Curry, Integers are encoded as binary values,
  being represented by constructor terms.

  (c) Holger Siegel 2009
-}
module Curry.ExtendedFlat.CurryArithmetics
    (CurryInt(..), CurryNat(..),
     trNat, trInt,
     toCurryInt, toIntExpression,
    ) where

import Curry.ExtendedFlat.Type


data CurryInt = Neg CurryNat | Zero | Pos CurryNat
data CurryNat = IHi | O CurryNat | I CurryNat


trNat :: Integral n =>
         a -> (a -> a) -> (a -> a) ->
         n -> a
trNat h o i = go
    where go n | n `mod` 2 == 0 = o (go m)
               | m == 0         = h
               | otherwise      = i (go m)
              where m = n `div` 2


trInt :: Integral n =>
         (nat -> t) -> t -> (nat -> t) ->
         nat -> (nat -> nat) -> (nat -> nat) ->
         n -> t
trInt n z p h o i = go
    where go x = case compare x 0 of
                   LT -> n (trNat h o i (negate x))
                   EQ -> z
                   GT -> p (trNat h o i x)


toCurryInt :: Integral a => a -> CurryInt
toCurryInt = trInt Neg Zero Pos IHi O I


toIntExpression :: Integral a => a -> Expr
toIntExpression = trInt neg_ zero_ pos_ iHi_ o_ i_


zero_, iHi_ :: Expr
pos_, neg_, o_, i_ :: Expr -> Expr

zero_  = prelCons tInt0 "Zero" []
pos_ n = prelCons tInt1 "Pos" [n]
neg_ n = prelCons tInt1 "Neg" [n]

iHi_ = prelCons tNat0 "IHi" []
o_ n = prelCons tNat1 "O" [n]
i_ n = prelCons tNat1 "I" [n]


tInt0, tInt1, tNat0, tNat1 :: TypeExpr
tInt0 = prelType "Int"
tInt1 = FuncType tInt0 tInt0

tNat0 = prelType "Nat"
tNat1 = FuncType tNat0 tNat0


prelType :: String -> TypeExpr
prelType s = TCons (mkQName ("Prelude", s)) []


prelCons :: TypeExpr -> String -> [Expr] -> Expr
prelCons t = Comb ConsCall . QName Nothing (Just t) "Prelude"