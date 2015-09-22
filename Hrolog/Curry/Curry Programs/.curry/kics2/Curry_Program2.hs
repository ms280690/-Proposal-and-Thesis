{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}


module Curry_Program2 (d_C_my_append) where

import Basics
import qualified Curry_Prelude

d_C_my_append :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Success
d_C_my_append x1 x2 x3 cd cs = case x1 of
     Curry_Prelude.OP_List -> Curry_Prelude.d_OP_eq_colon_eq x2 x3 cd cs
     (Curry_Prelude.OP_Cons x4 x5) -> d_C__case_0 x2 x5 x4 x3 cd cs
     (Curry_Prelude.Choice_OP_List d i l r) -> narrow d i (d_C_my_append l x2 x3 cd cs) (d_C_my_append r x2 x3 cd cs)
     (Curry_Prelude.Choices_OP_List d i xs) -> narrows cs d i (\z -> d_C_my_append z x2 x3 cd cs) xs
     (Curry_Prelude.Guard_OP_List d c e) -> guardCons d c ((d_C_my_append e x2 x3 cd) $! (addCs c cs))
     (Curry_Prelude.Fail_OP_List d info) -> failCons d (traceFail "Program2.my_append" [(show x1),(show x2),(show x3)] info)
     _ -> failCons cd (consFail "Program2.my_append" (showCons x1))

d_C__case_0 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Success
d_C__case_0 x2 x5 x4 x3 cd cs = case x3 of
     (Curry_Prelude.OP_Cons x6 x7) -> d_C___cond_0__case_0 x5 x2 x7 (Curry_Prelude.d_OP_eq_colon_eq x4 x6 cd cs) cd cs
     (Curry_Prelude.Choice_OP_List d i l r) -> narrow d i (d_C__case_0 x2 x5 x4 l cd cs) (d_C__case_0 x2 x5 x4 r cd cs)
     (Curry_Prelude.Choices_OP_List d i xs) -> narrows cs d i (\z -> d_C__case_0 x2 x5 x4 z cd cs) xs
     (Curry_Prelude.Guard_OP_List d c e) -> guardCons d c ((d_C__case_0 x2 x5 x4 e cd) $! (addCs c cs))
     (Curry_Prelude.Fail_OP_List d info) -> failCons d (traceFail "Program2._case_0" [(show x2),(show x5),(show x4),(show x3)] info)
     _ -> failCons cd (consFail "Program2._case_0" (showCons x3))

d_C___cond_0__case_0 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Curry_Prelude.C_Success -> Cover -> ConstStore -> Curry_Prelude.C_Success
d_C___cond_0__case_0 x1 x2 x3 x4 cd cs = case x4 of
     Curry_Prelude.C_Success -> d_C_my_append x1 x2 x3 cd cs
     (Curry_Prelude.Choice_C_Success d i l r) -> narrow d i (d_C___cond_0__case_0 x1 x2 x3 l cd cs) (d_C___cond_0__case_0 x1 x2 x3 r cd cs)
     (Curry_Prelude.Choices_C_Success d i xs) -> narrows cs d i (\z -> d_C___cond_0__case_0 x1 x2 x3 z cd cs) xs
     (Curry_Prelude.Guard_C_Success d c e) -> guardCons d c ((d_C___cond_0__case_0 x1 x2 x3 e cd) $! (addCs c cs))
     (Curry_Prelude.Fail_C_Success d info) -> failCons d (traceFail "Program2.__cond_0__case_0" [(show x1),(show x2),(show x3),(show x4)] info)
     _ -> failCons cd (consFail "Program2.__cond_0__case_0" (showCons x4))