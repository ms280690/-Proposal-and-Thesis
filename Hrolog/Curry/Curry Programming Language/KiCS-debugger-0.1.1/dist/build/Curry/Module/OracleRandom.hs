{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleRandom (module Curry.Module.OracleRandom) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Random
import Curry.Module.Prelude
import Curry.Module.System
import Curry.Module.Time
import Curry.Module.OraclePrelude
import Curry.Module.OracleSystem
import Curry.Module.OracleTime



-- begin included



-- end included

c_multiplier :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_multiplier x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))))))))))))))))))))))))))))(st)



c_addend :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addend x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st)



c_powermask :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_powermask x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))(st)



c_mask :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_mask x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))))))))))))))))))))))))))))))))))))))))))(st)



c_intsize :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_intsize x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))(st)



c_intspan :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_intspan x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))))))))))))))))))))))))))(st)



c_intlimit :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_intlimit x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))))))))))))))))))))))))(st)



c_sequence :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_sequence x2 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(let {x3 = Curry.Module.OracleRandom.c_nextseed(x2)(x1)(st)} in Curry.Module.CEventOracle.c_replace(x4)((Curry.Module.Prelude.:<)(x3)(Curry.Module.OracleRandom.c_sequence(x3)(x4)(st)))(st))(st)



c_nextseed :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_nextseed x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_mod(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_42(x2)(Curry.Module.OracleRandom.c_multiplier(x1)(st))(x3)(st))(Curry.Module.OracleRandom.c_addend(x4)(st))(x5)(st))(Curry.Module.OracleRandom.c_mask(x6)(st))(x7)(st))(st)



c_xor :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_xor x2 x3 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))))(let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))(Curry.Module.OracleRandom.c__case_12(x2)(x3)(Curry.Module.OracleRandom.c__case_11(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.OraclePrelude.c_mod(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x6)(st))(x7)(st))(x8)(st))(Curry.Module.OracleRandom.c_xor(Curry.Module.OraclePrelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x9)(st))(Curry.Module.OraclePrelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x10)(st))(x11)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(x12)(st))(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(x13)(st))(x14)(st))(x15)(st))(st))(st))(st)



c_power :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_power x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c_power'46binary'4643(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x2)(x3)(x1)(st))(st)



c_power'46binary'4643 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_power'46binary'4643 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleRandom.c__case_10(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Zero)(x1)(st))(x5)(st))(st)



c_nextIntBits :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nextIntBits x2 x3 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))))(Curry.Module.CEventOracle.c_replace(x14)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRandom.c_nextIntBits'46adjust'4653(Curry.Module.OracleRandom.c_power(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.OraclePrelude.op_45(Curry.Module.OracleRandom.c_powermask(x11)(st))(x3)(x12)(st))(x13)(st))))))(Curry.Module.OracleRandom.c_sequence(Curry.Module.OraclePrelude.c_mod(Curry.Module.OracleRandom.c_xor(x2)(Curry.Module.OracleRandom.c_multiplier(x1)(st))(x7)(st))(Curry.Module.OracleRandom.c_mask(x8)(st))(x9)(st))(x10)(st))(x14)(st))(st))(st))(st))(st)



c_nextIntBits'46adjust'4653 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_nextIntBits'46adjust'4653 x2 x3 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))(let {x4 = Curry.Module.OraclePrelude.c_mod(Curry.Module.OraclePrelude.c_div(x3)(x2)(x1)(st))(Curry.Module.OracleRandom.c_intspan(x5)(st))(x6)(st)} in let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OracleRandom.c__case_8(x4)(Curry.Module.OraclePrelude.op_62(x4)(Curry.Module.OracleRandom.c_intlimit(x7)(st))(x8)(st))(x9)(st))(st))(st)



c_nextInt :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nextInt x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleRandom.c_nextIntBits(x2)(Curry.Module.OracleRandom.c_intsize(x1)(st))(x3)(st))(st)



c_nextIntRange :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nextIntRange x2 x3 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))(let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleRandom.c__case_7(x3)(Curry.Module.OracleRandom.c_nextIntBits(x2)(Curry.Module.OraclePrelude.op_45(Curry.Module.OracleRandom.c_intsize(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x5)(st))(x6)(st))(Curry.Module.OraclePrelude.op_62(x3)(Curry.Module.Prelude.C_Zero)(x7)(st))(x8)(st))(st))(st)



c_nextIntRange'46adjust_a'4664 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_nextIntRange'46adjust_a'4664 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_div(Curry.Module.OraclePrelude.op_42(x2)(x3)(x1)(st))(Curry.Module.OracleRandom.c_intlimit(x4)(st))(x5)(st))(st)



c_nextIntRange'46adjust_b'4664 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_nextIntRange'46adjust_b'4664 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_mod(x3)(x2)(x1)(st))(st)



c_nextIntRange'46adjust_c'4664 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_nextIntRange'46adjust_c'4664 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_62_61(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(x3)(Curry.Module.OraclePrelude.c_mod(x3)(x2)(x1)(st))(x4)(st))(Curry.Module.OraclePrelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x5)(st))(x6)(st))(Curry.Module.Prelude.C_Zero)(x7)(st))(st)



c_nextIntRange'46power_of_2'4664 :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_nextIntRange'46power_of_2'4664 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x3)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x4)(st))(Curry.Module.Prelude.C_Zero)(x5)(st))(Curry.Module.OracleRandom.c_nextIntRange'46power_of_2'4664(Curry.Module.OraclePrelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x6)(st))(x7)(st))(x8)(st))(x9)(st))(x10)(st))(st)



c_nextBoolean :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Bool
c_nextBoolean x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(Curry.Module.Prelude.C_Zero)))))(Curry.Module.OracleRandom.c_nextIntBits(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x3)(st))(st)



c_getRandomSeed :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int))
c_getRandomSeed x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleTime.c_getClockTime(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRandom.c_getRandomSeed'46_'35lambda2))))(x2)(st))(st)



c_getRandomSeed'46_'35lambda2 :: Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int))
c_getRandomSeed'46_'35lambda2 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleSystem.c_getCPUTime(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRandom.c_getRandomSeed'46_'35lambda2'46_'35lambda3(x2)))))(x3)(st))(st)



c_getRandomSeed'46_'35lambda2'46_'35lambda3 :: Curry.Module.Time.C_ClockTime -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int))
c_getRandomSeed'46_'35lambda2'46_'35lambda3 x2 x3 x1 st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleTime.c_toUTCTime(x2)(x1)(st)} in let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.c_return(Curry.Module.OraclePrelude.c_mod(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_43(Curry.Module.OracleRandom.c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP3'35y(x4)(x11)(st))(Curry.Module.OracleRandom.c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP4'35mo(x4)(x12)(st))(x17)(st))(Curry.Module.OracleRandom.c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP5'35d(x4)(x13)(st))(x18)(st))(Curry.Module.OracleRandom.c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP6'35h(x4)(x14)(st))(x19)(st))(Curry.Module.OraclePrelude.op_42(Curry.Module.OraclePrelude.op_42(Curry.Module.OracleRandom.c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP7'35m(x4)(x15)(st))(Curry.Module.OracleRandom.c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP8'35s(x4)(x16)(st))(x20)(st))(x3)(x21)(st))(x22)(st))(Curry.Module.OracleRandom.c_mask(x23)(st))(x24)(st))(x25)(st))(st))(st))(st))(st))(st))(st))(st))(st)



c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP3'35y :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP3'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_5(x2)(x1)(st))(st)



c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP4'35mo :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP4'35mo x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_4(x2)(x1)(st))(st)



c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP5'35d :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP5'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_3(x2)(x1)(st))(st)



c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP6'35h :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP6'35h x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_2(x2)(x1)(st))(st)



c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP7'35m :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP7'35m x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_1(x2)(x1)(st))(st)



c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP8'35s :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_getRandomSeed'46_'35lambda2'46_'35lambda3'46_'35selFP8'35s x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_0(x2)(x1)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_0_case__12(x1)(x2)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_1_case__11(x1)(x2)(st))(st)



c__case_2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_2_case__10(x1)(x2)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_3_case__9(x1)(x2)(st))(st)



c__case_4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_4_case__8(x1)(x2)(st))(st)



c__case_5 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_5_case__7(x1)(x2)(st))(st)



c__case_7 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_7_case__6(x1)(x3)(x4)(x5)(st))(st)



c__case_6 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_6_case__5(x1)(x3)(x4)(x5)(st))(st)



c__case_8 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_8_case__4(x1)(x4)(x5)(st))(st)



c__case_10 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_10_case__3(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_9 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_9_case__2(x1)(x3)(x5)(st))(st)



c__case_11 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_11_case__1(x1)(x4)(st))(st)



c__case_12 x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandom.c__case_12_case__0(x1)(x4)(x5)(x6)(st))(st)



c__case_12_case__0 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_12_case__0 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x5)(x1)(st))(x7)(st))(st)
c__case_12_case__0 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_12_case__0(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_12_case__0 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_12_case__0")(x)



c__case_11_case__1 x1 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_11_case__1 x1 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_11_case__1 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_11_case__1(x1)(x)(st))(i)(xs)(st)
c__case_11_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_11_case__1")(x)



c__case_9_case__2 x1 x3 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_9_case__2 x1 x3 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_9_case__2 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_9_case__2(x1)(x3)(x)(st))(i)(xs)(st)
c__case_9_case__2 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_9_case__2")(x)



c__case_10_case__3 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_10_case__3 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))))))(Curry.Module.OracleRandom.c_power'46binary'4643(Curry.Module.OraclePrelude.op_42(x2)(Curry.Module.OracleRandom.c__case_9(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x6)(st))(x7)(st))(x8)(st))(Curry.Module.OraclePrelude.op_42(x3)(x3)(x9)(st))(Curry.Module.OraclePrelude.c_div(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x10)(st))(x11)(st))(st)
c__case_10_case__3 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_10_case__3(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_10_case__3 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_10_case__3")(x)



c__case_8_case__4 x1 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_45(x4)(Curry.Module.OracleRandom.c_intspan(x1)(st))(x6)(st))(st)
c__case_8_case__4 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_8_case__4 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_8_case__4(x1)(x4)(x)(st))(i)(xs)(st)
c__case_8_case__4 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_8_case__4")(x)



c__case_6_case__5 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRandom.c_nextIntRange'46adjust_a'4664(x3)))))(x4)(x1)(st))(st)
c__case_6_case__5 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRandom.c_nextIntRange'46adjust_b'4664(x3)))))(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRandom.c_nextIntRange'46adjust_c'4664(x3)))))(x4)(x1)(st))(x6)(st))(st)
c__case_6_case__5 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_6_case__5(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_6_case__5 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_6_case__5")(x)



c__case_7_case__6 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleRandom.c__case_6(x3)(x4)(Curry.Module.OracleRandom.c_nextIntRange'46power_of_2'4664(x3)(x1)(st))(x6)(st))(st)
c__case_7_case__6 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_7_case__6 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_7_case__6(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_7_case__6 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_7_case__6")(x)



c__case_5_case__7 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_5_case__7 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_5_case__7(x1)(x)(st))(i)(xs)(st)
c__case_5_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_5_case__7")(x)



c__case_4_case__8 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_4_case__8 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_4_case__8(x1)(x)(st))(i)(xs)(st)
c__case_4_case__8 x1 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_4_case__8")(x)



c__case_3_case__9 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_3_case__9 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_3_case__9(x1)(x)(st))(i)(xs)(st)
c__case_3_case__9 x1 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_3_case__9")(x)



c__case_2_case__10 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_2_case__10 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_2_case__10(x1)(x)(st))(i)(xs)(st)
c__case_2_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_2_case__10")(x)



c__case_1_case__11 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_1_case__11 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_1_case__11(x1)(x)(st))(i)(xs)(st)
c__case_1_case__11 x1 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_1_case__11")(x)



c__case_0_case__12 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_0_case__12 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandom.c__case_0_case__12(x1)(x)(st))(i)(xs)(st)
c__case_0_case__12 x1 x st = Curry.RunTimeSystem.patternFail("OracleRandom._case_0_case__12")(x)


