{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Float (module Curry.Module.Float) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



instance Fractional C_Float where
  fromRational x = PrimValue (fromRational x)
  recip (PrimValue x) = PrimValue (recip x)

instance Floating C_Float where
  pi = PrimValue pi
  exp (PrimValue x) = PrimValue (exp x)
  log (PrimValue x) = PrimValue (log x)
  sin (PrimValue x) = PrimValue (sin x)
  cos (PrimValue x) = PrimValue (cos x)
  sinh (PrimValue x) = PrimValue (sinh x)
  cosh (PrimValue x) = PrimValue (cosh x)
  asin (PrimValue x) = PrimValue (asin x)
  acos (PrimValue x) = PrimValue (acos x)
  atan (PrimValue x) = PrimValue (atan x)
  asinh (PrimValue x) = PrimValue (asinh x)
  acosh (PrimValue x) = PrimValue (acosh x)
  atanh (PrimValue x) = PrimValue (atanh x)
  
instance RealFrac C_Float where
  properFraction (PrimValue x) = case properFraction x of (b,a) -> (b,PrimValue a)

prim_Float_plus :: C_Float -> C_Float -> Result C_Float
prim_Float_plus x y _ = x+y

prim_Float_minus :: C_Float -> C_Float -> Result C_Float
prim_Float_minus x y _ = x-y

prim_Float_times :: C_Float -> C_Float -> Result C_Float
prim_Float_times x y _ = x*y

prim_Float_divide :: C_Float -> C_Float -> Result C_Float
prim_Float_divide x y _ = x/y

prim_Float_lt :: C_Float -> C_Float -> Result C_Bool
prim_Float_lt x y _ = toCurry (x<y)

prim_Float_gt :: C_Float -> C_Float -> Result C_Bool
prim_Float_gt x y _ = toCurry (x>y)

prim_Float_leq :: C_Float -> C_Float -> Result C_Bool
prim_Float_leq x y _ = toCurry (x<=y)

prim_Float_geq :: C_Float -> C_Float -> Result C_Bool
prim_Float_geq x y _ = toCurry (x>=y)

prim_i2f :: C_Int -> Result C_Float
prim_i2f x _ = fromInteger (fromCurry x)

prim_truncate :: C_Float -> Result C_Int
prim_truncate x _ = toCurry (truncate x :: Integer)

prim_round :: C_Float  -> Result C_Int
prim_round x _ = toCurry (round x :: Integer)

prim_sqrt :: C_Float  -> Result C_Float
prim_sqrt x _ = sqrt x

prim_log :: C_Float   -> Result C_Float
prim_log x _ = log x

prim_exp :: C_Float   -> Result C_Float
prim_exp x _ = exp x

prim_sin :: C_Float   -> Result C_Float
prim_sin x _ = sin x

prim_cos :: C_Float   -> Result C_Float
prim_cos x _ = cos x

prim_tan :: C_Float   -> Result C_Float
prim_tan x _ = tan x

prim_atan :: C_Float   -> Result C_Float
prim_atan x _ = atan x



-- end included

op_43_46 :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
op_43_46 x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Float.c_prim_Float_plus))(x1)(st))(x2)(st)



op_45_46 :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
op_45_46 x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Float.c_prim_Float_minus))(x1)(st))(x2)(st)



op_42_46 :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
op_42_46 x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Float.c_prim_Float_times))(x1)(st))(x2)(st)



op_47_46 :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
op_47_46 x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Float.c_prim_Float_divide))(x1)(st))(x2)(st)



op_60_46 :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60_46 x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Float.c_prim_Float_lt))(x1)(st))(x2)(st)



op_62_46 :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62_46 x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Float.c_prim_Float_gt))(x1)(st))(x2)(st)



op_60_61_46 :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60_61_46 x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Float.c_prim_Float_leq))(x1)(st))(x2)(st)



op_62_61_46 :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62_61_46 x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Float.c_prim_Float_geq))(x1)(st))(x2)(st)



c_i2f :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_i2f x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_i2f))(x1)(st)



c_truncate :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_truncate x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_truncate))(x1)(st)



c_round :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_round x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_round))(x1)(st)



c_sqrt :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_sqrt x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_sqrt))(x1)(st)



c_log :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_log x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_log))(x1)(st)



c_exp :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_exp x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_exp))(x1)(st)



c_sin :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_sin x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_sin))(x1)(st)



c_cos :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_cos x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_cos))(x1)(st)



c_tan :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_tan x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_tan))(x1)(st)



c_atan :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_atan x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Float.c_prim_atan))(x1)(st)



c_prim_Float_plus :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_Float_plus x1 x2 st = Curry.Module.Float.prim_Float_plus(x1)(x2)(st)



c_prim_Float_minus :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_Float_minus x1 x2 st = Curry.Module.Float.prim_Float_minus(x1)(x2)(st)



c_prim_Float_times :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_Float_times x1 x2 st = Curry.Module.Float.prim_Float_times(x1)(x2)(st)



c_prim_Float_divide :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_Float_divide x1 x2 st = Curry.Module.Float.prim_Float_divide(x1)(x2)(st)



c_prim_Float_lt :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_prim_Float_lt x1 x2 st = Curry.Module.Float.prim_Float_lt(x1)(x2)(st)



c_prim_Float_gt :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_prim_Float_gt x1 x2 st = Curry.Module.Float.prim_Float_gt(x1)(x2)(st)



c_prim_Float_leq :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_prim_Float_leq x1 x2 st = Curry.Module.Float.prim_Float_leq(x1)(x2)(st)



c_prim_Float_geq :: Curry.Module.Prelude.C_Float -> Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_prim_Float_geq x1 x2 st = Curry.Module.Float.prim_Float_geq(x1)(x2)(st)



c_prim_i2f :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_i2f x1 st = Curry.Module.Float.prim_i2f(x1)(st)



c_prim_truncate :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_prim_truncate x1 st = Curry.Module.Float.prim_truncate(x1)(st)



c_prim_round :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_prim_round x1 st = Curry.Module.Float.prim_round(x1)(st)



c_prim_sqrt :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_sqrt x1 st = Curry.Module.Float.prim_sqrt(x1)(st)



c_prim_log :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_log x1 st = Curry.Module.Float.prim_log(x1)(st)



c_prim_exp :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_exp x1 st = Curry.Module.Float.prim_exp(x1)(st)



c_prim_sin :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_sin x1 st = Curry.Module.Float.prim_sin(x1)(st)



c_prim_cos :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_cos x1 st = Curry.Module.Float.prim_cos(x1)(st)



c_prim_tan :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_tan x1 st = Curry.Module.Float.prim_tan(x1)(st)



c_prim_atan :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_atan x1 st = Curry.Module.Float.prim_atan(x1)(st)


