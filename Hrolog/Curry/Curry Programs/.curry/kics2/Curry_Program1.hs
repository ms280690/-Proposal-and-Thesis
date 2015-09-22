{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}


module Curry_Program1 (C_Stack (..), d_C_append1, d_C_top, d_C_pop, d_C_member, nd_C_insert, d_C_main) where

import Basics
import qualified Curry_Prelude

data C_Stack t0
     = C_Empty
     | C_Push t0 (C_Stack t0)
     | Choice_C_Stack Cover ID (C_Stack t0) (C_Stack t0)
     | Choices_C_Stack Cover ID ([C_Stack t0])
     | Fail_C_Stack Cover FailInfo
     | Guard_C_Stack Cover Constraints (C_Stack t0)

instance Show t0 => Show (C_Stack t0) where
  showsPrec d (Choice_C_Stack cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Stack cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Stack cd c e) = showsGuard d cd c e
  showsPrec _ (Fail_C_Stack cd info) = showChar '!'
  showsPrec _ C_Empty = showString "Empty"
  showsPrec _ (C_Push x1 x2) = (showString "(Push") . ((showChar ' ') . ((shows x1) . ((showChar ' ') . ((shows x2) . (showChar ')')))))


instance Read t0 => Read (C_Stack t0) where
  readsPrec d s = (readParen False (\r -> [ (C_Empty,r0) | (_,r0) <- readQualified "Program1" "Empty" r]) s) ++ (readParen (d > 10) (\r -> [ (C_Push x1 x2,r2) | (_,r0) <- readQualified "Program1" "Push" r, (x1,r1) <- readsPrec 11 r0, (x2,r2) <- readsPrec 11 r1]) s)


instance NonDet (C_Stack t0) where
  choiceCons = Choice_C_Stack
  choicesCons = Choices_C_Stack
  failCons = Fail_C_Stack
  guardCons = Guard_C_Stack
  try (Choice_C_Stack cd i x y) = tryChoice cd i x y
  try (Choices_C_Stack cd i xs) = tryChoices cd i xs
  try (Fail_C_Stack cd info) = Fail cd info
  try (Guard_C_Stack cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Stack cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Stack cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Stack cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Stack cd i _) = error ("Program1.Stack.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Stack cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Stack cd c e) = f cd c e
  match _ _ _ _ _ f x = f x


instance Generable t0 => Generable (C_Stack t0) where
  generate s c = Choices_C_Stack c (freeID [0,2] s) [C_Empty,(C_Push (generate (leftSupply s) c) (generate (rightSupply s) c))]


instance NormalForm t0 => NormalForm (C_Stack t0) where
  ($!!) cont C_Empty d cs = cont C_Empty d cs
  ($!!) cont (C_Push x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Push y1 y2) d cs) $!! x2) d) cs) $!! x1) d) cs
  ($!!) cont (Choice_C_Stack cd i x y) d cs = nfChoice cont cd i x y cd cs
  ($!!) cont (Choices_C_Stack cd i xs) d cs = nfChoices cont cd i xs d cs
  ($!!) cont (Guard_C_Stack cd c e) d cs = guardCons cd c (((cont $!! e) d) (addCs c cs))
  ($!!) _ (Fail_C_Stack cd info) _ _ = failCons cd info
  ($##) cont C_Empty d cs = cont C_Empty d cs
  ($##) cont (C_Push x1 x2) d cs = (((\y1 d cs -> (((\y2 d cs -> cont (C_Push y1 y2) d cs) $## x2) d) cs) $## x1) d) cs
  ($##) cont (Choice_C_Stack cd i x y) d cs = gnfChoice cont cd i x y cd cs
  ($##) cont (Choices_C_Stack cd i xs) d cs = gnfChoices cont cd i xs d cs
  ($##) cont (Guard_C_Stack cd c e) d cs = guardCons cd c (((cont $## e) d) (addCs c cs))
  ($##) _ (Fail_C_Stack cd info) _ _ = failCons cd info
  showCons C_Empty = "Program1.Empty"
  showCons (C_Push _ _) = "Program1.Push _ _"
  showCons x = error ("Program1.Stack.showCons: no constructor: " ++ (show x))
  searchNF _ cont C_Empty = cont C_Empty
  searchNF search cont (C_Push x1 x2) = search (\y1 -> search (\y2 -> cont (C_Push y1 y2)) x2) x1
  searchNF _ _ x = error ("Program1.Stack.searchNF: no constructor: " ++ (show x))


instance Unifiable t0 => Unifiable (C_Stack t0) where
  (=.=) C_Empty C_Empty d cs = C_Success
  (=.=) (C_Push x1 x2) (C_Push y1 y2) d cs = (((((x1 =:= y1) d) cs) & (((x2 =:= y2) d) cs)) d) cs
  (=.=) a b cd _ = Fail_C_Success cd (unificationFail (showCons a) (showCons b))
  (=.<=) C_Empty C_Empty d cs = C_Success
  (=.<=) (C_Push x1 x2) (C_Push y1 y2) d cs = (((((x1 =:<= y1) d) cs) & (((x2 =:<= y2) d) cs)) d) cs
  (=.<=) a b cd _ = Fail_C_Success cd (unificationFail (showCons a) (showCons b))
  bind cd i C_Empty = ((i :=: (ChooseN 0 0)):(concat []))
  bind cd i (C_Push x3 x4) = ((i :=: (ChooseN 1 2)):(concat [(bind cd (leftID i) x3),(bind cd (rightID i) x4)]))
  bind d i (Choice_C_Stack cd j x y) = [(ConstraintChoice cd j (bind d i x) (bind d i y))]
  bind d i (Choices_C_Stack cd j@(FreeID _ _) xs) = bindOrNarrow d i cd j xs
  bind d i (Choices_C_Stack cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind d i) xs))]
  bind _ _ (Choices_C_Stack cd i _) = error ("Program1.Stack.bind: Choices with ChoiceID: " ++ (show i))
  bind _ _ (Fail_C_Stack cd info) = [(Unsolvable info)]
  bind d i (Guard_C_Stack cd c e) = (getConstrList c) ++ (bind d i e)
  lazyBind cd i C_Empty = [(i :=: (ChooseN 0 0))]
  lazyBind cd i (C_Push x3 x4) = [(i :=: (ChooseN 1 2)),((leftID i) :=: (LazyBind (lazyBind cd (leftID i) x3))),((rightID i) :=: (LazyBind (lazyBind cd (rightID i) x4)))]
  lazyBind d i (Choice_C_Stack cd j x y) = [(ConstraintChoice cd j (lazyBind d i x) (lazyBind d i y))]
  lazyBind d i (Choices_C_Stack cd j@(FreeID _ _) xs) = lazyBindOrNarrow d i cd j xs
  lazyBind d i (Choices_C_Stack cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind d i) xs))]
  lazyBind _ _ (Choices_C_Stack cd i _) = error ("Program1.Stack.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ _ (Fail_C_Stack cd info) = [(Unsolvable info)]
  lazyBind d i (Guard_C_Stack cd c e) = (getConstrList c) ++ [(i :=: (LazyBind (lazyBind d i e)))]


instance Curry_Prelude.Curry t0 => Curry_Prelude.Curry (C_Stack t0) where
  (=?=) (Choice_C_Stack cd i x y) z d cs = narrow cd i (((x Curry_Prelude.=?= z) d) cs) (((y Curry_Prelude.=?= z) d) cs)
  (=?=) (Choices_C_Stack cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.=?= y) d) cs) xs
  (=?=) (Guard_C_Stack cd c e) y d cs = guardCons cd c (((e Curry_Prelude.=?= y) d) (addCs c cs))
  (=?=) (Fail_C_Stack cd info) _ _ _ = failCons cd info
  (=?=) z (Choice_C_Stack cd i x y) d cs = narrow cd i (((z Curry_Prelude.=?= x) d) cs) (((z Curry_Prelude.=?= y) d) cs)
  (=?=) y (Choices_C_Stack cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.=?= x) d) cs) xs
  (=?=) y (Guard_C_Stack cd c e) d cs = guardCons cd c (((y Curry_Prelude.=?= e) d) (addCs c cs))
  (=?=) _ (Fail_C_Stack cd info) _ _ = failCons cd info
  (=?=) C_Empty C_Empty d cs = Curry_Prelude.C_True
  (=?=) (C_Push x1 x2) (C_Push y1 y2) d cs = Curry_Prelude.d_OP_amp_amp (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.=?= y2) d) cs) d cs
  (=?=) _ _ d _ = Curry_Prelude.C_False
  (<?=) (Choice_C_Stack cd i x y) z d cs = narrow cd i (((x Curry_Prelude.<?= z) d) cs) (((y Curry_Prelude.<?= z) d) cs)
  (<?=) (Choices_C_Stack cd i xs) y d cs = narrows cs cd i (\x -> ((x Curry_Prelude.<?= y) d) cs) xs
  (<?=) (Guard_C_Stack cd c e) y d cs = guardCons cd c (((e Curry_Prelude.<?= y) d) (addCs c cs))
  (<?=) (Fail_C_Stack cd info) _ _ _ = failCons cd info
  (<?=) z (Choice_C_Stack cd i x y) d cs = narrow cd i (((z Curry_Prelude.<?= x) d) cs) (((z Curry_Prelude.<?= y) d) cs)
  (<?=) y (Choices_C_Stack cd i xs) d cs = narrows cs cd i (\x -> ((y Curry_Prelude.<?= x) d) cs) xs
  (<?=) y (Guard_C_Stack cd c e) d cs = guardCons cd c (((y Curry_Prelude.<?= e) d) (addCs c cs))
  (<?=) _ (Fail_C_Stack cd info) _ _ = failCons cd info
  (<?=) C_Empty C_Empty d cs = Curry_Prelude.C_True
  (<?=) C_Empty (C_Push _ _) _ _ = Curry_Prelude.C_True
  (<?=) (C_Push x1 x2) (C_Push y1 y2) d cs = Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_lt x1 y1 d cs) (Curry_Prelude.d_OP_amp_amp (((x1 Curry_Prelude.=?= y1) d) cs) (((x2 Curry_Prelude.<?= y2) d) cs) d cs) d cs
  (<?=) _ _ d _ = Curry_Prelude.C_False


d_C_append1 :: Curry_Prelude.Curry t0 => Curry_Prelude.OP_List t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
d_C_append1 x1 x2 cd cs = case x1 of
     Curry_Prelude.OP_List -> x2
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_OP_plus_plus (Curry_Prelude.OP_Cons x3 Curry_Prelude.OP_List) (d_C_append1 x4 x2 cd cs) cd cs
     (Curry_Prelude.Choice_OP_List d i l r) -> narrow d i (d_C_append1 l x2 cd cs) (d_C_append1 r x2 cd cs)
     (Curry_Prelude.Choices_OP_List d i xs) -> narrows cs d i (\z -> d_C_append1 z x2 cd cs) xs
     (Curry_Prelude.Guard_OP_List d c e) -> guardCons d c ((d_C_append1 e x2 cd) $! (addCs c cs))
     (Curry_Prelude.Fail_OP_List d info) -> failCons d (traceFail "Program1.append1" [(show x1),(show x2)] info)
     _ -> failCons cd (consFail "Program1.append1" (showCons x1))

d_C_top :: Curry_Prelude.Curry t0 => C_Stack (C_Stack t0) -> Cover -> ConstStore -> C_Stack t0
d_C_top x1 cd cs = case x1 of
     C_Empty -> C_Empty
     (C_Push x2 x3) -> x2
     (Choice_C_Stack d i l r) -> narrow d i (d_C_top l cd cs) (d_C_top r cd cs)
     (Choices_C_Stack d i xs) -> narrows cs d i (\z -> d_C_top z cd cs) xs
     (Guard_C_Stack d c e) -> guardCons d c ((d_C_top e cd) $! (addCs c cs))
     (Fail_C_Stack d info) -> failCons d (traceFail "Program1.top" [(show x1)] info)
     _ -> failCons cd (consFail "Program1.top" (showCons x1))

d_C_pop :: Curry_Prelude.Curry t0 => C_Stack t0 -> Cover -> ConstStore -> C_Stack t0
d_C_pop x1 cd cs = case x1 of
     C_Empty -> C_Empty
     (C_Push x2 x3) -> x3
     (Choice_C_Stack d i l r) -> narrow d i (d_C_pop l cd cs) (d_C_pop r cd cs)
     (Choices_C_Stack d i xs) -> narrows cs d i (\z -> d_C_pop z cd cs) xs
     (Guard_C_Stack d c e) -> guardCons d c ((d_C_pop e cd) $! (addCs c cs))
     (Fail_C_Stack d info) -> failCons d (traceFail "Program1.pop" [(show x1)] info)
     _ -> failCons cd (consFail "Program1.pop" (showCons x1))

d_C_member :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> Cover -> ConstStore -> Curry_Prelude.C_Bool
d_C_member x1 x2 cd cs = case x2 of
     Curry_Prelude.OP_List -> Curry_Prelude.C_False
     (Curry_Prelude.OP_Cons x3 x4) -> Curry_Prelude.d_OP_bar_bar (Curry_Prelude.d_OP_eq_eq x1 x3 cd cs) (d_C_member x1 x4 cd cs) cd cs
     (Curry_Prelude.Choice_OP_List d i l r) -> narrow d i (d_C_member x1 l cd cs) (d_C_member x1 r cd cs)
     (Curry_Prelude.Choices_OP_List d i xs) -> narrows cs d i (\z -> d_C_member x1 z cd cs) xs
     (Curry_Prelude.Guard_OP_List d c e) -> guardCons d c ((d_C_member x1 e cd) $! (addCs c cs))
     (Curry_Prelude.Fail_OP_List d info) -> failCons d (traceFail "Program1.member" [(show x1),(show x2)] info)
     _ -> failCons cd (consFail "Program1.member" (showCons x2))

nd_C_insert :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C_insert x1 x2 s cd cs = let
     s2 = s
      in (seq s2 (let
          s1 = leftSupply s2
          s0 = rightSupply s2
           in (seq s1 (seq s0 (choice (let
                in (nd_C__case_0 x1 x2 s0 cd cs)) (Curry_Prelude.OP_Cons x1 x2) s1 cd cs)))))

d_C_main :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
d_C_main cd cs = Curry_Prelude.d_C_print (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'H'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'e'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'l'#) (Curry_Prelude.OP_Cons (Curry_Prelude.C_Char 'o'#) Curry_Prelude.OP_List))))) cd cs

nd_C__case_0 :: Curry_Prelude.Curry t0 => t0 -> Curry_Prelude.OP_List t0 -> IDSupply -> Cover -> ConstStore -> Curry_Prelude.OP_List t0
nd_C__case_0 x1 x2 s cd cs = case x2 of
     (Curry_Prelude.OP_Cons x3 x4) -> let
          s0 = s
           in (seq s0 (let
                in (Curry_Prelude.OP_Cons x3 (let
                     in (nd_C_insert x1 x4 s0 cd cs)))))
     (Curry_Prelude.Choice_OP_List d i l r) -> narrow d i (nd_C__case_0 x1 l s cd cs) (nd_C__case_0 x1 r s cd cs)
     (Curry_Prelude.Choices_OP_List d i xs) -> narrows cs d i (\z -> nd_C__case_0 x1 z s cd cs) xs
     (Curry_Prelude.Guard_OP_List d c e) -> guardCons d c ((nd_C__case_0 x1 e s cd) $! (addCs c cs))
     (Curry_Prelude.Fail_OP_List d info) -> failCons d (traceFail "Program1._case_0" [(show x1),(show x2)] info)
     _ -> failCons cd (consFail "Program1._case_0" (showCons x2))