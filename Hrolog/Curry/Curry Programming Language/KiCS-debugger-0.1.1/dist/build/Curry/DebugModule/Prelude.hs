{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Prelude where
import qualified Char
import qualified Curry.Debugger.ShowTerm as ST
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
 
instance DI.GenTerm Float where
        genTerm (FloatUnderscore)
          = DI.TermUnderscore (DI.SrcID "Prelude" 2)
        genTerm (Float f) = DI.TermFloat f
 
instance DI.GenTerm Char where
        genTerm (CharUnderscore) = DI.TermUnderscore (DI.SrcID "Prelude" 0)
        genTerm (Char c) = DI.TermChar c
 
instance DI.GenTerm (IO dm a) where
        genTerm (IOUnderscore)
          = DI.TermUnderscore (DI.SrcID "Prelude" Prelude.undefined)
        genTerm x0 = Prelude.error "not implemented"
 
natToHInt :: Nat -> Prelude.Int
natToHInt (IHi) = 1
natToHInt (O x) = 2 Prelude.* natToHInt x
natToHInt (I x) = 2 Prelude.* natToHInt x Prelude.+ 1
 
intToHInt :: Int -> Prelude.Int
intToHInt (Neg n) = Prelude.negate (natToHInt n)
intToHInt (Pos n) = natToHInt n
intToHInt (Zero) = 0
 
hIntToNat :: (Prelude.Integral n) => n -> Nat
hIntToNat 1 = IHi
hIntToNat i
  = case Prelude.divMod i 2 of
        (d, 0) -> O (hIntToNat d)
        (d, 1) -> I (hIntToNat d)
 
hIntToInt :: (Prelude.Integral n) => n -> Int
hIntToInt i
  | i Prelude.< 0 = Neg (hIntToNat (Prelude.negate i))
  | i Prelude.== 0 = Zero
  | Prelude.otherwise = Pos (hIntToNat i)
 
listToHList :: List a -> [a]
listToHList (Nil) = []
listToHList (Cons x xs) = x : listToHList xs
 
hListToList :: [a] -> List a
hListToList [] = Nil
hListToList (x : xs) = (Cons x (hListToList xs))
 
charToHChar :: Char -> Prelude.Char
charToHChar (Char c) = c
 
hCharToChar :: Prelude.Char -> Char
hCharToChar c = Char c
 
hStrToStr :: Prelude.String -> List Char
hStrToStr str = hListToList (Prelude.map hCharToChar str)
 
strToHStr :: List Char -> Prelude.String
strToHStr listChar = Prelude.map charToHChar (listToHList listChar)
 
data Float = Float Prelude.Float
           | FloatUnderscore
           deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Char = Char Prelude.Char
          | CharUnderscore
          deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data World = World
 
data (DM.DM dm) => IO dm a = IO (World -> dm (a, World))
                           | IOUnderscore
 
instance Data.Generics.Typeable (IO dm a)
 
instance Data.Generics.Data (IO dm a)
 
return :: (DM.DM dm) => a -> dm a
return = Prelude.return
(.) = (Prelude..)
($) = (Prelude.$)
 
curryReturn :: (DM.DM dm) => a -> dm (IO dm a)
curryReturn x = return (IO (\ w -> return (x, w)))
 
(?) :: (DM.DM dm, DI.GenTerm a) => a -> a -> dm a
x ? y = x DM.? y
 
strict_prim_putChar :: (DM.DM dm) => Char -> dm (IO dm Unit)
strict_prim_putChar x0
  = hook_strict_prim_putChar x0 (curryReturn Unit)
 
strict_getChar :: (DM.DM dm) => dm (IO dm Char)
strict_getChar
  = hook_strict_getChar
      (do c <- DM.getNextExtVal
          curryReturn (Char c))
 
op_DollarEMark ::
               (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                 DM.Func dm a b -> a -> dm b
op_DollarEMark x0 x1 = hook_op_DollarEMark x0 x1 (curryApply x0 x1)
 
op_DollarEMarkEMark ::
                    (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                      DM.Func dm a b -> a -> dm b
op_DollarEMarkEMark x0 x1
  = hook_op_DollarEMarkEMark x0 x1 (curryApply x0 x1)
 
op_DollarRhomb ::
               (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                 DM.Func dm a b -> a -> dm b
op_DollarRhomb x0 x1 = hook_op_DollarRhomb x0 x1 (curryApply x0 x1)
 
op_DollarRhombRhomb ::
                    (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                      DM.Func dm a b -> a -> dm b
op_DollarRhombRhomb x0 x1
  = hook_op_DollarRhombRhomb x0 x1 (curryApply x0 x1)
 
strict_prim_error :: (DM.DM dm, DI.GenTerm a) => List Char -> dm a
strict_prim_error x0
  = hook_strict_prim_error x0
      (DM.errorHook (Prelude.map charToHChar (listToHList x0)))
 
strict_failed :: (DM.DM dm, DI.GenTerm a) => dm a
strict_failed = hook_strict_failed (return DM.failed)
 
op_EqEq :: (DM.DM dm, DI.GenTerm a) => a -> a -> dm Bool
op_EqEq x0 x1 = hook_op_EqEq x0 x1 (x0 `eqeq` x1)
 
eqeq :: (DM.DM dm, DI.GenTerm a) => a -> a -> dm Bool
eqeq x0 x1 = DM.treatCase' Prelude.False (eqeqx x0) x1
  where eqeqx x y
          | DI.genTerm x Prelude.== DI.genTerm y = return True
          | Prelude.otherwise = return False
 
strict_prim_ord :: (DM.DM dm) => Char -> dm Int
strict_prim_ord x0@(Char c)
  = hook_strict_prim_ord x0 (return (hIntToInt (Char.ord c)))
 
strict_prim_chr :: (DM.DM dm) => Int -> dm Char
strict_prim_chr x0
  = hook_strict_prim_chr x0 (return (Char (Char.chr (intToHInt x0))))
 
op_EqEqEq :: (DM.DM dm, DI.GenTerm a) => a -> a -> dm Bool
op_EqEqEq x0 x1 = hook_op_EqEqEq x0 x1 (x0 `eqeq` x1)
 
op_And :: (DM.DM dm) => Success -> Success -> dm Success
op_And x0 x1 = hook_op_And x0 x1 (Prelude.error "not implemented")
 
op_GtGtEq ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
            IO dm a -> DM.Func dm a (IO dm b) -> dm (IO dm b)
op_GtGtEq a1@(IO a) k
  = hook_op_GtGtEq a1 k
      (return
         (IO
            (\ w ->
               do DM.popOracle
                  (r, w') <- a w
                  DM.popOracle
                  IO f <- curryApply k r
                  DM.popOracle
                  f w')))
 
strict_return :: (DM.DM dm, DI.GenTerm a) => a -> dm (IO dm a)
strict_return x = hook_strict_return x (curryReturn x)
 
strict_prim_readFile ::
                     (DM.DM dm) => List Char -> dm (IO dm (List Char))
strict_prim_readFile x0
  = hook_strict_prim_readFile x0
      (do f <- DM.getNextExtVal
          curryReturn (hStrToStr f))
 
strict_prim_writeFile ::
                      (DM.DM dm) => List Char -> List Char -> dm (IO dm Unit)
strict_prim_writeFile x0 x1
  = hook_strict_prim_writeFile x0 x1 (curryReturn Unit)
 
strict_prim_appendFile ::
                       (DM.DM dm) => List Char -> List Char -> dm (IO dm Unit)
strict_prim_appendFile x0 x1
  = hook_strict_prim_appendFile x0 x1 (curryReturn Unit)
 
strict_catchFail ::
                 (DM.DM dm, DI.GenTerm a) => IO dm a -> IO dm a -> dm (IO dm a)
strict_catchFail x0 x1
  = hook_strict_catchFail x0 x1 (Prelude.error "not implemented")
 
strict_prim_show :: (DM.DM dm, DI.GenTerm a) => a -> dm (List Char)
strict_prim_show x0 = hook_strict_prim_show x0 (show x0)
show x0 = DM.treatCase' Prelude.False (return . show') x0
  where show' x = hStrToStr hStr
          where hStr = ST.showGenTerm x
 
strict_getSearchTree ::
                     (DM.DM dm, DI.GenTerm a) => a -> dm (IO dm (SearchTree a))
strict_getSearchTree x0
  = hook_strict_getSearchTree x0 (Prelude.error "not implemented")
 
curryApply :: (DM.DM dm) => DM.Func dm a b -> a -> dm b
curryApply (DM.FuncRep _ f) x = f x
 
strict_apply ::
             (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
               DM.Func dm a b -> a -> dm b
strict_apply f x = hook_strict_apply f x (curryApply f x)
 
strict_cond :: (DM.DM dm, DI.GenTerm a) => Success -> a -> dm a
strict_cond x0 x1
  = hook_strict_cond x0 x1 (Prelude.error "not implemented")
 
op_EqColonLtEq :: (DM.DM dm, DI.GenTerm a) => a -> a -> dm Success
op_EqColonLtEq x0 x1
  = hook_op_EqColonLtEq x0 x1 (Prelude.error "not implemented")
 
instance (Data.Generics.Typeable a, Data.Generics.Typeable b,
          Data.Generics.Typeable c, Data.Generics.Typeable d,
          Data.Generics.Typeable e, Data.Generics.Typeable f,
          Data.Generics.Typeable g, Data.Generics.Typeable h) =>
         Data.Generics.Typeable (Tuple8 a b c d e f g h) where
        typeOf _
          = Data.Generics.mkTyConApp
              (Data.Generics.mkTyCon "Curry.DebugModule.Prelude.Tuple8")
              [Data.Generics.typeOf ta, Data.Generics.typeOf tb,
               Data.Generics.typeOf tc, Data.Generics.typeOf td,
               Data.Generics.typeOf te, Data.Generics.typeOf tf,
               Data.Generics.typeOf tg, Data.Generics.typeOf th]
          where  
                ta :: a
                ta = Prelude.undefined
                 
                tb :: b
                tb = Prelude.undefined
                 
                tc :: c
                tc = Prelude.undefined
                 
                td :: d
                td = Prelude.undefined
                 
                te :: e
                te = Prelude.undefined
                 
                tf :: f
                tf = Prelude.undefined
                 
                tg :: g
                tg = Prelude.undefined
                 
                th :: h
                th = Prelude.undefined
 
instance (Data.Generics.Data a, Data.Generics.Data b,
          Data.Generics.Data c, Data.Generics.Data d, Data.Generics.Data e,
          Data.Generics.Data f, Data.Generics.Data g,
          Data.Generics.Data h) =>
         Data.Generics.Data (Tuple8 a b c d e f g h) where
        gfoldl k z (Tuple8 x1 x2 x3 x4 x5 x6 x7 x8)
          = k (k (k (k (k (k (k (k (z Tuple8) x1) x2) x3) x4) x5) x6) x7) x8
        gunfold k z c
          = case Data.Generics.constrIndex c of
                1 -> k (k (k (k (k (k (k (k (z Tuple8))))))))
        toConstr (Tuple8 _ _ _ _ _ _ _ _) = con_Tuple8
        dataTypeOf _ = ty_Tuple8
 
instance (Data.Generics.Typeable a, Data.Generics.Typeable b,
          Data.Generics.Typeable c, Data.Generics.Typeable d,
          Data.Generics.Typeable e, Data.Generics.Typeable f,
          Data.Generics.Typeable g, Data.Generics.Typeable h,
          Data.Generics.Typeable i) =>
         Data.Generics.Typeable (Tuple9 a b c d e f g h i) where
        typeOf _
          = Data.Generics.mkTyConApp
              (Data.Generics.mkTyCon "Curry.DebugModule.Prelude.Tuple9")
              [Data.Generics.typeOf ta, Data.Generics.typeOf tb,
               Data.Generics.typeOf tc, Data.Generics.typeOf td,
               Data.Generics.typeOf te, Data.Generics.typeOf tf,
               Data.Generics.typeOf tg, Data.Generics.typeOf th,
               Data.Generics.typeOf ti]
          where  
                ta :: a
                ta = Prelude.undefined
                 
                tb :: b
                tb = Prelude.undefined
                 
                tc :: c
                tc = Prelude.undefined
                 
                td :: d
                td = Prelude.undefined
                 
                te :: e
                te = Prelude.undefined
                 
                tf :: f
                tf = Prelude.undefined
                 
                tg :: g
                tg = Prelude.undefined
                 
                th :: h
                th = Prelude.undefined
                 
                ti :: i
                ti = Prelude.undefined
 
instance (Data.Generics.Data a, Data.Generics.Data b,
          Data.Generics.Data c, Data.Generics.Data d, Data.Generics.Data e,
          Data.Generics.Data f, Data.Generics.Data g, Data.Generics.Data h,
          Data.Generics.Data i) =>
         Data.Generics.Data (Tuple9 a b c d e f g h i) where
        gfoldl k z (Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9)
          = k
              (k (k (k (k (k (k (k (k (z Tuple9) x1) x2) x3) x4) x5) x6) x7) x8)
              x9
        gunfold k z c
          = case Data.Generics.constrIndex c of
                1 -> k (k (k (k (k (k (k (k (k (z Tuple9)))))))))
        toConstr (Tuple9 _ _ _ _ _ _ _ _ _) = con_Tuple9
        dataTypeOf _ = ty_Tuple9
 
instance (Data.Generics.Typeable a, Data.Generics.Typeable b,
          Data.Generics.Typeable c, Data.Generics.Typeable d,
          Data.Generics.Typeable e, Data.Generics.Typeable f,
          Data.Generics.Typeable g, Data.Generics.Typeable h,
          Data.Generics.Typeable i, Data.Generics.Typeable j) =>
         Data.Generics.Typeable (Tuple10 a b c d e f g h i j) where
        typeOf _
          = Data.Generics.mkTyConApp
              (Data.Generics.mkTyCon "Curry.DebugModule.Prelude.Tuple10")
              [Data.Generics.typeOf ta, Data.Generics.typeOf tb,
               Data.Generics.typeOf tc, Data.Generics.typeOf td,
               Data.Generics.typeOf te, Data.Generics.typeOf tf,
               Data.Generics.typeOf tg, Data.Generics.typeOf th,
               Data.Generics.typeOf ti, Data.Generics.typeOf tj]
          where  
                ta :: a
                ta = Prelude.undefined
                 
                tb :: b
                tb = Prelude.undefined
                 
                tc :: c
                tc = Prelude.undefined
                 
                td :: d
                td = Prelude.undefined
                 
                te :: e
                te = Prelude.undefined
                 
                tf :: f
                tf = Prelude.undefined
                 
                tg :: g
                tg = Prelude.undefined
                 
                th :: h
                th = Prelude.undefined
                 
                ti :: i
                ti = Prelude.undefined
                 
                tj :: j
                tj = Prelude.undefined
 
instance (Data.Generics.Data a, Data.Generics.Data b,
          Data.Generics.Data c, Data.Generics.Data d, Data.Generics.Data e,
          Data.Generics.Data f, Data.Generics.Data g, Data.Generics.Data h,
          Data.Generics.Data i, Data.Generics.Data j) =>
         Data.Generics.Data (Tuple10 a b c d e f g h i j) where
        gfoldl k z (Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
          = k
              (k
                 (k (k (k (k (k (k (k (k (z Tuple10) x1) x2) x3) x4) x5) x6) x7) x8)
                 x9)
              x10
        gunfold k z c
          = case Data.Generics.constrIndex c of
                1 -> k (k (k (k (k (k (k (k (k (k (z Tuple10))))))))))
        toConstr (Tuple10 _ _ _ _ _ _ _ _ _ _) = con_Tuple10
        dataTypeOf _ = ty_Tuple10
 
instance (Data.Generics.Typeable a, Data.Generics.Typeable b,
          Data.Generics.Typeable c, Data.Generics.Typeable d,
          Data.Generics.Typeable e, Data.Generics.Typeable f,
          Data.Generics.Typeable g, Data.Generics.Typeable h,
          Data.Generics.Typeable i, Data.Generics.Typeable j,
          Data.Generics.Typeable k) =>
         Data.Generics.Typeable (Tuple11 a b c d e f g h i j k) where
        typeOf _
          = Data.Generics.mkTyConApp
              (Data.Generics.mkTyCon "Curry.DebugModule.Prelude.Tuple11")
              [Data.Generics.typeOf ta, Data.Generics.typeOf tb,
               Data.Generics.typeOf tc, Data.Generics.typeOf td,
               Data.Generics.typeOf te, Data.Generics.typeOf tf,
               Data.Generics.typeOf tg, Data.Generics.typeOf th,
               Data.Generics.typeOf ti, Data.Generics.typeOf tj,
               Data.Generics.typeOf tk]
          where  
                ta :: a
                ta = Prelude.undefined
                 
                tb :: b
                tb = Prelude.undefined
                 
                tc :: c
                tc = Prelude.undefined
                 
                td :: d
                td = Prelude.undefined
                 
                te :: e
                te = Prelude.undefined
                 
                tf :: f
                tf = Prelude.undefined
                 
                tg :: g
                tg = Prelude.undefined
                 
                th :: h
                th = Prelude.undefined
                 
                ti :: i
                ti = Prelude.undefined
                 
                tj :: j
                tj = Prelude.undefined
                 
                tk :: k
                tk = Prelude.undefined
 
instance (Data.Generics.Data a, Data.Generics.Data b,
          Data.Generics.Data c, Data.Generics.Data d, Data.Generics.Data e,
          Data.Generics.Data f, Data.Generics.Data g, Data.Generics.Data h,
          Data.Generics.Data i, Data.Generics.Data j,
          Data.Generics.Data k) =>
         Data.Generics.Data (Tuple11 a b c d e f g h i j k) where
        gfoldl k z (Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)
          = k
              (k
                 (k
                    (k (k (k (k (k (k (k (k (z Tuple11) x1) x2) x3) x4) x5) x6) x7) x8)
                    x9)
                 x10)
              x11
        gunfold k z c
          = case Data.Generics.constrIndex c of
                1 -> k (k (k (k (k (k (k (k (k (k (k (z Tuple11)))))))))))
        toConstr (Tuple11 _ _ _ _ _ _ _ _ _ _ _) = con_Tuple11
        dataTypeOf _ = ty_Tuple11
 
instance (Data.Generics.Typeable a, Data.Generics.Typeable b,
          Data.Generics.Typeable c, Data.Generics.Typeable d,
          Data.Generics.Typeable e, Data.Generics.Typeable f,
          Data.Generics.Typeable g, Data.Generics.Typeable h,
          Data.Generics.Typeable i, Data.Generics.Typeable j,
          Data.Generics.Typeable k, Data.Generics.Typeable l) =>
         Data.Generics.Typeable (Tuple12 a b c d e f g h i j k l) where
        typeOf _
          = Data.Generics.mkTyConApp
              (Data.Generics.mkTyCon "Curry.DebugModule.Prelude.Tuple12")
              [Data.Generics.typeOf ta, Data.Generics.typeOf tb,
               Data.Generics.typeOf tc, Data.Generics.typeOf td,
               Data.Generics.typeOf te, Data.Generics.typeOf tf,
               Data.Generics.typeOf tg, Data.Generics.typeOf th,
               Data.Generics.typeOf ti, Data.Generics.typeOf tj,
               Data.Generics.typeOf tk, Data.Generics.typeOf tl]
          where  
                ta :: a
                ta = Prelude.undefined
                 
                tb :: b
                tb = Prelude.undefined
                 
                tc :: c
                tc = Prelude.undefined
                 
                td :: d
                td = Prelude.undefined
                 
                te :: e
                te = Prelude.undefined
                 
                tf :: f
                tf = Prelude.undefined
                 
                tg :: g
                tg = Prelude.undefined
                 
                th :: h
                th = Prelude.undefined
                 
                ti :: i
                ti = Prelude.undefined
                 
                tj :: j
                tj = Prelude.undefined
                 
                tk :: k
                tk = Prelude.undefined
                 
                tl :: l
                tl = Prelude.undefined
 
instance (Data.Generics.Data a, Data.Generics.Data b,
          Data.Generics.Data c, Data.Generics.Data d, Data.Generics.Data e,
          Data.Generics.Data f, Data.Generics.Data g, Data.Generics.Data h,
          Data.Generics.Data i, Data.Generics.Data j, Data.Generics.Data k,
          Data.Generics.Data l) =>
         Data.Generics.Data (Tuple12 a b c d e f g h i j k l) where
        gfoldl k z (Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)
          = k
              (k
                 (k
                    (k
                       (k (k (k (k (k (k (k (k (z Tuple12) x1) x2) x3) x4) x5) x6) x7) x8)
                       x9)
                    x10)
                 x11)
              x12
        gunfold k z c
          = case Data.Generics.constrIndex c of
                1 -> k (k (k (k (k (k (k (k (k (k (k (k (z Tuple12))))))))))))
        toConstr (Tuple12 _ _ _ _ _ _ _ _ _ _ _ _) = con_Tuple12
        dataTypeOf _ = ty_Tuple12
 
instance (Data.Generics.Typeable a, Data.Generics.Typeable b,
          Data.Generics.Typeable c, Data.Generics.Typeable d,
          Data.Generics.Typeable e, Data.Generics.Typeable f,
          Data.Generics.Typeable g, Data.Generics.Typeable h,
          Data.Generics.Typeable i, Data.Generics.Typeable j,
          Data.Generics.Typeable k, Data.Generics.Typeable l,
          Data.Generics.Typeable m) =>
         Data.Generics.Typeable (Tuple13 a b c d e f g h i j k l m) where
        typeOf _
          = Data.Generics.mkTyConApp
              (Data.Generics.mkTyCon "Curry.DebugModule.Prelude.Tuple13")
              [Data.Generics.typeOf ta, Data.Generics.typeOf tb,
               Data.Generics.typeOf tc, Data.Generics.typeOf td,
               Data.Generics.typeOf te, Data.Generics.typeOf tf,
               Data.Generics.typeOf tg, Data.Generics.typeOf th,
               Data.Generics.typeOf ti, Data.Generics.typeOf tj,
               Data.Generics.typeOf tk, Data.Generics.typeOf tl,
               Data.Generics.typeOf tm]
          where  
                ta :: a
                ta = Prelude.undefined
                 
                tb :: b
                tb = Prelude.undefined
                 
                tc :: c
                tc = Prelude.undefined
                 
                td :: d
                td = Prelude.undefined
                 
                te :: e
                te = Prelude.undefined
                 
                tf :: f
                tf = Prelude.undefined
                 
                tg :: g
                tg = Prelude.undefined
                 
                th :: h
                th = Prelude.undefined
                 
                ti :: i
                ti = Prelude.undefined
                 
                tj :: j
                tj = Prelude.undefined
                 
                tk :: k
                tk = Prelude.undefined
                 
                tl :: l
                tl = Prelude.undefined
                 
                tm :: m
                tm = Prelude.undefined
 
instance (Data.Generics.Data a, Data.Generics.Data b,
          Data.Generics.Data c, Data.Generics.Data d, Data.Generics.Data e,
          Data.Generics.Data f, Data.Generics.Data g, Data.Generics.Data h,
          Data.Generics.Data i, Data.Generics.Data j, Data.Generics.Data k,
          Data.Generics.Data l, Data.Generics.Data m) =>
         Data.Generics.Data (Tuple13 a b c d e f g h i j k l m) where
        gfoldl k z (Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)
          = k
              (k
                 (k
                    (k
                       (k
                          (k (k (k (k (k (k (k (k (z Tuple13) x1) x2) x3) x4) x5) x6) x7) x8)
                          x9)
                       x10)
                    x11)
                 x12)
              x13
        gunfold k z c
          = case Data.Generics.constrIndex c of
                1 -> k (k (k (k (k (k (k (k (k (k (k (k (k (z Tuple13)))))))))))))
        toConstr (Tuple13 _ _ _ _ _ _ _ _ _ _ _ _ _) = con_Tuple13
        dataTypeOf _ = ty_Tuple13
 
instance (Data.Generics.Typeable a, Data.Generics.Typeable b,
          Data.Generics.Typeable c, Data.Generics.Typeable d,
          Data.Generics.Typeable e, Data.Generics.Typeable f,
          Data.Generics.Typeable g, Data.Generics.Typeable h,
          Data.Generics.Typeable i, Data.Generics.Typeable j,
          Data.Generics.Typeable k, Data.Generics.Typeable l,
          Data.Generics.Typeable m, Data.Generics.Typeable n) =>
         Data.Generics.Typeable (Tuple14 a b c d e f g h i j k l m n) where
        typeOf _
          = Data.Generics.mkTyConApp
              (Data.Generics.mkTyCon "Curry.DebugModule.Prelude.Tuple14")
              [Data.Generics.typeOf ta, Data.Generics.typeOf tb,
               Data.Generics.typeOf tc, Data.Generics.typeOf td,
               Data.Generics.typeOf te, Data.Generics.typeOf tf,
               Data.Generics.typeOf tg, Data.Generics.typeOf th,
               Data.Generics.typeOf ti, Data.Generics.typeOf tj,
               Data.Generics.typeOf tk, Data.Generics.typeOf tl,
               Data.Generics.typeOf tm, Data.Generics.typeOf tn]
          where  
                ta :: a
                ta = Prelude.undefined
                 
                tb :: b
                tb = Prelude.undefined
                 
                tc :: c
                tc = Prelude.undefined
                 
                td :: d
                td = Prelude.undefined
                 
                te :: e
                te = Prelude.undefined
                 
                tf :: f
                tf = Prelude.undefined
                 
                tg :: g
                tg = Prelude.undefined
                 
                th :: h
                th = Prelude.undefined
                 
                ti :: i
                ti = Prelude.undefined
                 
                tj :: j
                tj = Prelude.undefined
                 
                tk :: k
                tk = Prelude.undefined
                 
                tl :: l
                tl = Prelude.undefined
                 
                tm :: m
                tm = Prelude.undefined
                 
                tn :: n
                tn = Prelude.undefined
 
instance (Data.Generics.Data a, Data.Generics.Data b,
          Data.Generics.Data c, Data.Generics.Data d, Data.Generics.Data e,
          Data.Generics.Data f, Data.Generics.Data g, Data.Generics.Data h,
          Data.Generics.Data i, Data.Generics.Data j, Data.Generics.Data k,
          Data.Generics.Data l, Data.Generics.Data m,
          Data.Generics.Data n) =>
         Data.Generics.Data (Tuple14 a b c d e f g h i j k l m n) where
        gfoldl k z (Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)
          = k
              (k
                 (k
                    (k
                       (k
                          (k
                             (k (k (k (k (k (k (k (k (z Tuple14) x1) x2) x3) x4) x5) x6) x7) x8)
                             x9)
                          x10)
                       x11)
                    x12)
                 x13)
              x14
        gunfold k z c
          = case Data.Generics.constrIndex c of
                1
                  -> k
                       (k (k (k (k (k (k (k (k (k (k (k (k (k (z Tuple14))))))))))))))
        toConstr (Tuple14 _ _ _ _ _ _ _ _ _ _ _ _ _ _) = con_Tuple14
        dataTypeOf _ = ty_Tuple14
 
instance (Data.Generics.Typeable a, Data.Generics.Typeable b,
          Data.Generics.Typeable c, Data.Generics.Typeable d,
          Data.Generics.Typeable e, Data.Generics.Typeable f,
          Data.Generics.Typeable g, Data.Generics.Typeable h,
          Data.Generics.Typeable i, Data.Generics.Typeable j,
          Data.Generics.Typeable k, Data.Generics.Typeable l,
          Data.Generics.Typeable m, Data.Generics.Typeable n,
          Data.Generics.Typeable o) =>
         Data.Generics.Typeable (Tuple15 a b c d e f g h i j k l m n o)
         where
        typeOf _
          = Data.Generics.mkTyConApp
              (Data.Generics.mkTyCon "Curry.DebugModule.Prelude.Tuple15")
              [Data.Generics.typeOf ta, Data.Generics.typeOf tb,
               Data.Generics.typeOf tc, Data.Generics.typeOf td,
               Data.Generics.typeOf te, Data.Generics.typeOf tf,
               Data.Generics.typeOf tg, Data.Generics.typeOf th,
               Data.Generics.typeOf ti, Data.Generics.typeOf tj,
               Data.Generics.typeOf tk, Data.Generics.typeOf tl,
               Data.Generics.typeOf tm, Data.Generics.typeOf tn,
               Data.Generics.typeOf to]
          where  
                ta :: a
                ta = Prelude.undefined
                 
                tb :: b
                tb = Prelude.undefined
                 
                tc :: c
                tc = Prelude.undefined
                 
                td :: d
                td = Prelude.undefined
                 
                te :: e
                te = Prelude.undefined
                 
                tf :: f
                tf = Prelude.undefined
                 
                tg :: g
                tg = Prelude.undefined
                 
                th :: h
                th = Prelude.undefined
                 
                ti :: i
                ti = Prelude.undefined
                 
                tj :: j
                tj = Prelude.undefined
                 
                tk :: k
                tk = Prelude.undefined
                 
                tl :: l
                tl = Prelude.undefined
                 
                tm :: m
                tm = Prelude.undefined
                 
                tn :: n
                tn = Prelude.undefined
                 
                to :: o
                to = Prelude.undefined
 
instance (Data.Generics.Data a, Data.Generics.Data b,
          Data.Generics.Data c, Data.Generics.Data d, Data.Generics.Data e,
          Data.Generics.Data f, Data.Generics.Data g, Data.Generics.Data h,
          Data.Generics.Data i, Data.Generics.Data j, Data.Generics.Data k,
          Data.Generics.Data l, Data.Generics.Data m, Data.Generics.Data n,
          Data.Generics.Data o) =>
         Data.Generics.Data (Tuple15 a b c d e f g h i j k l m n o) where
        gfoldl k z
          (Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)
          = k
              (k
                 (k
                    (k
                       (k
                          (k
                             (k
                                (k (k (k (k (k (k (k (k (z Tuple15) x1) x2) x3) x4) x5) x6) x7) x8)
                                x9)
                             x10)
                          x11)
                       x12)
                    x13)
                 x14)
              x15
        gunfold k z c
          = case Data.Generics.constrIndex c of
                1
                  -> k
                       (k (k (k (k (k (k (k (k (k (k (k (k (k (k (z Tuple15)))))))))))))))
        toConstr (Tuple15 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = con_Tuple15
        dataTypeOf _ = ty_Tuple15
 
instance DI.GenTerm Unit where
        genTerm (Unit) = DI.Term "()" (DI.SrcID "Prelude" 0) []
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a) => DI.GenTerm (List a) where
        genTerm (Nil) = DI.Term "[]" (DI.SrcID "Prelude" 0) []
        genTerm (Cons x1 x2)
          = DI.Term ":" (DI.SrcID "Prelude" 0) [DI.genTerm x1, DI.genTerm x2]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b) => DI.GenTerm (Tuple2 a b)
         where
        genTerm (Tuple2 x1 x2)
          = DI.Term "(,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) => DI.GenTerm
         (Tuple3 a b c) where
        genTerm (Tuple3 x1 x2 x3)
          = DI.Term "(,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c,
          DI.GenTerm d) =>
         DI.GenTerm (Tuple4 a b c d) where
        genTerm (Tuple4 x1 x2 x3 x4)
          = DI.Term "(,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e) =>
         DI.GenTerm (Tuple5 a b c d e) where
        genTerm (Tuple5 x1 x2 x3 x4 x5)
          = DI.Term "(,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f) =>
         DI.GenTerm (Tuple6 a b c d e f) where
        genTerm (Tuple6 x1 x2 x3 x4 x5 x6)
          = DI.Term "(,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f, DI.GenTerm g) =>
         DI.GenTerm (Tuple7 a b c d e f g) where
        genTerm (Tuple7 x1 x2 x3 x4 x5 x6 x7)
          = DI.Term "(,,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h) =>
         DI.GenTerm (Tuple8 a b c d e f g h) where
        genTerm (Tuple8 x1 x2 x3 x4 x5 x6 x7 x8)
          = DI.Term "(,,,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
          DI.GenTerm i) =>
         DI.GenTerm (Tuple9 a b c d e f g h i) where
        genTerm (Tuple9 x1 x2 x3 x4 x5 x6 x7 x8 x9)
          = DI.Term "(,,,,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
               DI.genTerm x9]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
          DI.GenTerm i, DI.GenTerm j) =>
         DI.GenTerm (Tuple10 a b c d e f g h i j) where
        genTerm (Tuple10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)
          = DI.Term "(,,,,,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
               DI.genTerm x9, DI.genTerm x10]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
          DI.GenTerm i, DI.GenTerm j, DI.GenTerm k) =>
         DI.GenTerm (Tuple11 a b c d e f g h i j k) where
        genTerm (Tuple11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)
          = DI.Term "(,,,,,,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
               DI.genTerm x9, DI.genTerm x10, DI.genTerm x11]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
          DI.GenTerm i, DI.GenTerm j, DI.GenTerm k, DI.GenTerm l) =>
         DI.GenTerm (Tuple12 a b c d e f g h i j k l) where
        genTerm (Tuple12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)
          = DI.Term "(,,,,,,,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
               DI.genTerm x9, DI.genTerm x10, DI.genTerm x11, DI.genTerm x12]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
          DI.GenTerm i, DI.GenTerm j, DI.GenTerm k, DI.GenTerm l,
          DI.GenTerm m) =>
         DI.GenTerm (Tuple13 a b c d e f g h i j k l m) where
        genTerm (Tuple13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)
          = DI.Term "(,,,,,,,,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
               DI.genTerm x9, DI.genTerm x10, DI.genTerm x11, DI.genTerm x12,
               DI.genTerm x13]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
          DI.GenTerm i, DI.GenTerm j, DI.GenTerm k, DI.GenTerm l,
          DI.GenTerm m, DI.GenTerm n) =>
         DI.GenTerm (Tuple14 a b c d e f g h i j k l m n) where
        genTerm (Tuple14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)
          = DI.Term "(,,,,,,,,,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
               DI.genTerm x9, DI.genTerm x10, DI.genTerm x11, DI.genTerm x12,
               DI.genTerm x13, DI.genTerm x14]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b, DI.GenTerm c, DI.GenTerm d,
          DI.GenTerm e, DI.GenTerm f, DI.GenTerm g, DI.GenTerm h,
          DI.GenTerm i, DI.GenTerm j, DI.GenTerm k, DI.GenTerm l,
          DI.GenTerm m, DI.GenTerm n, DI.GenTerm o) =>
         DI.GenTerm (Tuple15 a b c d e f g h i j k l m n o) where
        genTerm
          (Tuple15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)
          = DI.Term "(,,,,,,,,,,,,,,)" (DI.SrcID "Prelude" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
               DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
               DI.genTerm x9, DI.genTerm x10, DI.genTerm x11, DI.genTerm x12,
               DI.genTerm x13, DI.genTerm x14, DI.genTerm x15]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance DI.GenTerm Bool where
        genTerm (False) = DI.Term "False" (DI.SrcID "Prelude" 0) []
        genTerm (True) = DI.Term "True" (DI.SrcID "Prelude" 0) []
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance DI.GenTerm Ordering where
        genTerm (LT) = DI.Term "LT" (DI.SrcID "Prelude" 0) []
        genTerm (EQ) = DI.Term "EQ" (DI.SrcID "Prelude" 0) []
        genTerm (GT) = DI.Term "GT" (DI.SrcID "Prelude" 0) []
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance DI.GenTerm Nat where
        genTerm (IHi) = DI.Term "IHi" (DI.SrcID "Prelude" 0) []
        genTerm (O x1) = DI.Term "O" (DI.SrcID "Prelude" 0) [DI.genTerm x1]
        genTerm (I x1) = DI.Term "I" (DI.SrcID "Prelude" 0) [DI.genTerm x1]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance DI.GenTerm Int where
        genTerm (Neg x1)
          = DI.Term "Neg" (DI.SrcID "Prelude" 0) [DI.genTerm x1]
        genTerm (Zero) = DI.Term "Zero" (DI.SrcID "Prelude" 0) []
        genTerm (Pos x1)
          = DI.Term "Pos" (DI.SrcID "Prelude" 0) [DI.genTerm x1]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance DI.GenTerm Success where
        genTerm (Success) = DI.Term "Success" (DI.SrcID "Prelude" 0) []
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a) => DI.GenTerm (Maybe a) where
        genTerm (Nothing) = DI.Term "Nothing" (DI.SrcID "Prelude" 0) []
        genTerm (Just x1)
          = DI.Term "Just" (DI.SrcID "Prelude" 0) [DI.genTerm x1]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a, DI.GenTerm b) => DI.GenTerm (Either a b)
         where
        genTerm (Left x1)
          = DI.Term "Left" (DI.SrcID "Prelude" 0) [DI.genTerm x1]
        genTerm (Right x1)
          = DI.Term "Right" (DI.SrcID "Prelude" 0) [DI.genTerm x1]
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
instance (DI.GenTerm a) => DI.GenTerm (SearchTree a) where
        genTerm (Fail) = DI.Term "Fail" (DI.SrcID "Prelude" 0) []
        genTerm (Value x1)
          = DI.Term "Value" (DI.SrcID "Prelude" 0) [DI.genTerm x1]
        genTerm (Choice x1)
          = DI.Term "Choice" (DI.SrcID "Prelude" 0) [DI.genTerm x1]
        genTerm (Suspend) = DI.Term "Suspend" (DI.SrcID "Prelude" 0) []
        genTerm x1 = DM.genericTerm (DI.SrcID "Prelude" 0) x1
 
data Unit = UnitFail
          | UnitOr DM.OrRef [Unit]
          | UnitUnderscore
          | Unit
          deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data List a = ListFail
            | ListOr DM.OrRef [List a]
            | ListUnderscore
            | Nil
            | Cons a (List a)
            deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Tuple2 a b = Tuple2Fail
                | Tuple2Or DM.OrRef [Tuple2 a b]
                | Tuple2Underscore
                | Tuple2 a b
                deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Tuple3 a b c = Tuple3Fail
                  | Tuple3Or DM.OrRef [Tuple3 a b c]
                  | Tuple3Underscore
                  | Tuple3 a b c
                  deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Tuple4 a b c d = Tuple4Fail
                    | Tuple4Or DM.OrRef [Tuple4 a b c d]
                    | Tuple4Underscore
                    | Tuple4 a b c d
                    deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Tuple5 a b c d e = Tuple5Fail
                      | Tuple5Or DM.OrRef [Tuple5 a b c d e]
                      | Tuple5Underscore
                      | Tuple5 a b c d e
                      deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Tuple6 a b c d e f = Tuple6Fail
                        | Tuple6Or DM.OrRef [Tuple6 a b c d e f]
                        | Tuple6Underscore
                        | Tuple6 a b c d e f
                        deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Tuple7 a b c d e f g = Tuple7Fail
                          | Tuple7Or DM.OrRef [Tuple7 a b c d e f g]
                          | Tuple7Underscore
                          | Tuple7 a b c d e f g
                          deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Tuple8 a b c d e f g h = Tuple8Fail
                            | Tuple8Or DM.OrRef [Tuple8 a b c d e f g h]
                            | Tuple8Underscore
                            | Tuple8 a b c d e f g h
 
data Tuple9 a b c d e f g h i = Tuple9Fail
                              | Tuple9Or DM.OrRef [Tuple9 a b c d e f g h i]
                              | Tuple9Underscore
                              | Tuple9 a b c d e f g h i
 
data Tuple10 a b c d e f g h i j = Tuple10Fail
                                 | Tuple10Or DM.OrRef [Tuple10 a b c d e f g h i j]
                                 | Tuple10Underscore
                                 | Tuple10 a b c d e f g h i j
 
data Tuple11 a b c d e f g h i j k = Tuple11Fail
                                   | Tuple11Or DM.OrRef [Tuple11 a b c d e f g h i j k]
                                   | Tuple11Underscore
                                   | Tuple11 a b c d e f g h i j k
 
data Tuple12 a b c d e f g h i j k l = Tuple12Fail
                                     | Tuple12Or DM.OrRef [Tuple12 a b c d e f g h i j k l]
                                     | Tuple12Underscore
                                     | Tuple12 a b c d e f g h i j k l
 
data Tuple13 a b c d e f g h i j k l m = Tuple13Fail
                                       | Tuple13Or DM.OrRef [Tuple13 a b c d e f g h i j k l m]
                                       | Tuple13Underscore
                                       | Tuple13 a b c d e f g h i j k l m
 
data Tuple14 a b c d e f g h i j k l m n = Tuple14Fail
                                         | Tuple14Or DM.OrRef [Tuple14 a b c d e f g h i j k l m n]
                                         | Tuple14Underscore
                                         | Tuple14 a b c d e f g h i j k l m n
 
data Tuple15 a b c d e f g h i j k l m n o = Tuple15Fail
                                           | Tuple15Or DM.OrRef
                                                       [Tuple15 a b c d e f g h i j k l m n o]
                                           | Tuple15Underscore
                                           | Tuple15 a b c d e f g h i j k l m n o
 
data Bool = BoolFail
          | BoolOr DM.OrRef [Bool]
          | BoolUnderscore
          | False
          | True
          deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Ordering = OrderingFail
              | OrderingOr DM.OrRef [Ordering]
              | OrderingUnderscore
              | LT
              | EQ
              | GT
              deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Nat = NatFail
         | NatOr DM.OrRef [Nat]
         | NatUnderscore
         | IHi
         | O Nat
         | I Nat
         deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Int = IntFail
         | IntOr DM.OrRef [Int]
         | IntUnderscore
         | Neg Nat
         | Zero
         | Pos Nat
         deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Success = SuccessFail
             | SuccessOr DM.OrRef [Success]
             | SuccessUnderscore
             | Success
             deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Maybe a = MaybeFail
             | MaybeOr DM.OrRef [Maybe a]
             | MaybeUnderscore
             | Nothing
             | Just a
             deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data Either a b = EitherFail
                | EitherOr DM.OrRef [Either a b]
                | EitherUnderscore
                | Left a
                | Right b
                deriving (Data.Generics.Typeable, Data.Generics.Data)
 
data SearchTree a = SearchTreeFail
                  | SearchTreeOr DM.OrRef [SearchTree a]
                  | SearchTreeUnderscore
                  | Fail
                  | Value a
                  | Choice (List (SearchTree a))
                  | Suspend
                  deriving (Data.Generics.Typeable, Data.Generics.Data)
ty_Tuple8
  = Data.Generics.mkDataType "Curry.DebugModule.Prelude.Tuple8"
      [con_Tuple8]
con_Tuple8
  = Data.Generics.mkConstr ty_Tuple8 "Tuple8" [] Data.Generics.Prefix
ty_Tuple9
  = Data.Generics.mkDataType "Curry.DebugModule.Prelude.Tuple9"
      [con_Tuple9]
con_Tuple9
  = Data.Generics.mkConstr ty_Tuple9 "Tuple9" [] Data.Generics.Prefix
ty_Tuple10
  = Data.Generics.mkDataType "Curry.DebugModule.Prelude.Tuple10"
      [con_Tuple10]
con_Tuple10
  = Data.Generics.mkConstr ty_Tuple10 "Tuple10" []
      Data.Generics.Prefix
ty_Tuple11
  = Data.Generics.mkDataType "Curry.DebugModule.Prelude.Tuple11"
      [con_Tuple11]
con_Tuple11
  = Data.Generics.mkConstr ty_Tuple11 "Tuple11" []
      Data.Generics.Prefix
ty_Tuple12
  = Data.Generics.mkDataType "Curry.DebugModule.Prelude.Tuple12"
      [con_Tuple12]
con_Tuple12
  = Data.Generics.mkConstr ty_Tuple12 "Tuple12" []
      Data.Generics.Prefix
ty_Tuple13
  = Data.Generics.mkDataType "Curry.DebugModule.Prelude.Tuple13"
      [con_Tuple13]
con_Tuple13
  = Data.Generics.mkConstr ty_Tuple13 "Tuple13" []
      Data.Generics.Prefix
ty_Tuple14
  = Data.Generics.mkDataType "Curry.DebugModule.Prelude.Tuple14"
      [con_Tuple14]
con_Tuple14
  = Data.Generics.mkConstr ty_Tuple14 "Tuple14" []
      Data.Generics.Prefix
ty_Tuple15
  = Data.Generics.mkDataType "Curry.DebugModule.Prelude.Tuple15"
      [con_Tuple15]
con_Tuple15
  = Data.Generics.mkConstr ty_Tuple15 "Tuple15" []
      Data.Generics.Prefix
 
op_Point ::
         (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) =>
           DM.Func dm a b -> DM.Func dm c a -> dm (DM.Func dm c b)
op_Point x1 x2
  = DM.eval
      (DM.funcDeclHook "."
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             Prelude.return
               (PC.partCall1
                  (x'xterm_strict_4646_35lambda2 [DI.genTerm x3, DI.genTerm x4])
                  (x'xstrict_4646_35lambda2 x3 x4))))
term_op_Point x1 = DI.Term "." (DI.SrcID "Prelude" 0) x1
 
x'xstrict_4646_35lambda2 ::
                         (DM.DM dm, DI.GenTerm j, DI.GenTerm l, DI.GenTerm h) =>
                           DM.Func dm j l -> DM.Func dm h j -> h -> dm l
x'xstrict_4646_35lambda2 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook ".._#lambda2"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- Prelude.return x1
             x7 <- do x4 <- Prelude.return x2
                      x5 <- Prelude.return x3
                      DM.funcCallHook "apply"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (strict_apply x4 x5)
             DM.funcCallHook "apply"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
               (strict_apply x6 x7)))
x'xterm_strict_4646_35lambda2 x1
  = DI.Term ".._#lambda2" (DI.SrcID "Prelude" 0) x1
 
strict_id :: (DM.DM dm, DI.GenTerm a) => a -> dm a
strict_id x1
  = DM.eval
      (DM.funcDeclHook "id"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (Prelude.return x1))
term_strict_id x1 = DI.Term "id" (DI.SrcID "Prelude" 0) x1
 
strict_const ::
             (DM.DM dm, DI.GenTerm a, DI.GenTerm b) => a -> b -> dm a
strict_const x1 x2
  = DM.eval
      (DM.funcDeclHook "const"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (Prelude.return x1))
term_strict_const x1 = DI.Term "const" (DI.SrcID "Prelude" 0) x1
 
strict_curry ::
             (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) =>
               DM.Func dm (Tuple2 a b) c -> a -> b -> dm c
strict_curry x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "curry"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- Prelude.return x1
             x7 <- do x4 <- Prelude.return x2
                      x5 <- Prelude.return x3
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (Prelude.return (Tuple2 x4 x5))
             DM.funcCallHook "apply"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
               (strict_apply x6 x7)))
term_strict_curry x1 = DI.Term "curry" (DI.SrcID "Prelude" 0) x1
 
strict_uncurry ::
               (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) =>
                 DM.Func dm a (DM.Func dm b c) -> Tuple2 a b -> dm c
strict_uncurry x1 x2
  = DM.eval
      (DM.funcDeclHook "uncurry"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_155"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_155 x3 x4)))
term_strict_uncurry x1
  = DI.Term "uncurry" (DI.SrcID "Prelude" 0) x1
 
strict_flip ::
            (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) =>
              DM.Func dm a (DM.Func dm b c) -> b -> a -> dm c
strict_flip x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "flip"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- do x4 <- Prelude.return x1
                      x5 <- Prelude.return x3
                      DM.funcCallHook "apply"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (strict_apply x4 x5)
             x7 <- Prelude.return x2
             DM.funcCallHook "apply"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
               (strict_apply x6 x7)))
term_strict_flip x1 = DI.Term "flip" (DI.SrcID "Prelude" 0) x1
 
strict_until ::
             (DM.DM dm, DI.GenTerm a) =>
               DM.Func dm a Bool -> DM.Func dm a a -> a -> dm a
strict_until x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "until"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- Prelude.return x1
             x7 <- Prelude.return x2
             x8 <- Prelude.return x3
             x9 <- do x4 <- Prelude.return x1
                      x5 <- Prelude.return x3
                      DM.funcCallHook "apply"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (strict_apply x4 x5)
             DM.funcCallHook "_case_154"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
               (strict__case_154 x6 x7 x8 x9)))
term_strict_until x1 = DI.Term "until" (DI.SrcID "Prelude" 0) x1
 
op_Dollar ::
          (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
            DM.Func dm a b -> a -> dm b
op_Dollar x1 x2
  = DM.eval
      (DM.funcDeclHook "$"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "apply"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict_apply x3 x4)))
term_op_Dollar x1 = DI.Term "$" (DI.SrcID "Prelude" 0) x1
 
strict_ensureSpine ::
                   (DM.DM dm, DI.GenTerm a) => dm (DM.Func dm (List a) (List a))
strict_ensureSpine
  = DM.eval
      (DM.funcDeclHook "ensureSpine"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return
                     (PC.partCall1 (x'xterm_strict_ensureSpine46ensureList4621 [])
                        x'xstrict_ensureSpine46ensureList4621)
             Prelude.return
               (PC.partCall1 (term_op_DollarRhomb [DI.genTerm x0])
                  (op_DollarRhomb x0))))
 
x'xstrict_ensureSpine46ensureList4621 ::
                                      (DM.DM dm, DI.GenTerm a) => List a -> dm (List a)
x'xstrict_ensureSpine46ensureList4621 x1
  = DM.eval
      (DM.funcDeclHook "ensureSpine.ensureList.21"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_153"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_153 x2)))
x'xterm_strict_ensureSpine46ensureList4621 x1
  = DI.Term "ensureSpine.ensureList.21" (DI.SrcID "Prelude" 0) x1
 
strict_seq ::
           (DM.DM dm, DI.GenTerm a, DI.GenTerm b) => a -> b -> dm b
strict_seq x1 x2
  = DM.eval
      (DM.funcDeclHook "seq"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x4 <- do x3 <- Prelude.return x2
                      Prelude.return
                        (PC.partCall1 (term_strict_const [DI.genTerm x3])
                           (strict_const x3))
             x5 <- Prelude.return x1
             DM.funcCallHook "$!"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (op_DollarEMark x4 x5)))
term_strict_seq x1 = DI.Term "seq" (DI.SrcID "Prelude" 0) x1
 
strict_error :: (DM.DM dm, DI.GenTerm a) => List Char -> dm a
strict_error x1
  = DM.eval
      (DM.funcDeclHook "error"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_error []) strict_prim_error)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (op_DollarRhombRhomb x2 x3)))
term_strict_error x1 = DI.Term "error" (DI.SrcID "Prelude" 0) x1
 
op_AndAnd :: (DM.DM dm) => Bool -> Bool -> dm Bool
op_AndAnd x1 x2
  = DM.eval
      (DM.funcDeclHook "&&"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_152"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_152 x3 x4)))
term_op_AndAnd x1 = DI.Term "&&" (DI.SrcID "Prelude" 0) x1
 
op_OrOr :: (DM.DM dm) => Bool -> Bool -> dm Bool
op_OrOr x1 x2
  = DM.eval
      (DM.funcDeclHook "||"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_151"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_151 x3 x4)))
term_op_OrOr x1 = DI.Term "||" (DI.SrcID "Prelude" 0) x1
 
strict_not :: (DM.DM dm) => Bool -> dm Bool
strict_not x1
  = DM.eval
      (DM.funcDeclHook "not"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_150"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_150 x2)))
term_strict_not x1 = DI.Term "not" (DI.SrcID "Prelude" 0) x1
 
strict_otherwise :: (DM.DM dm) => dm Bool
strict_otherwise
  = DM.eval
      (DM.funcDeclHook "otherwise"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (DM.constructorHook
            (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
            (Prelude.return True)))
 
strict_if_then_else ::
                    (DM.DM dm, DI.GenTerm a) => Bool -> a -> a -> dm a
strict_if_then_else x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "if_then_else"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x2
             x5 <- Prelude.return x3
             x6 <- Prelude.return x1
             DM.funcCallHook "_case_149"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_149 x4 x5 x6)))
term_strict_if_then_else x1
  = DI.Term "if_then_else" (DI.SrcID "Prelude" 0) x1
 
strict_isLT :: (DM.DM dm) => Ordering -> dm Bool
strict_isLT x1
  = DM.eval
      (DM.funcDeclHook "isLT"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_148"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_148 x2)))
term_strict_isLT x1 = DI.Term "isLT" (DI.SrcID "Prelude" 0) x1
 
strict_isGT :: (DM.DM dm) => Ordering -> dm Bool
strict_isGT x1
  = DM.eval
      (DM.funcDeclHook "isGT"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_147"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_147 x2)))
term_strict_isGT x1 = DI.Term "isGT" (DI.SrcID "Prelude" 0) x1
 
strict_isEQ :: (DM.DM dm) => Ordering -> dm Bool
strict_isEQ x1
  = DM.eval
      (DM.funcDeclHook "isEQ"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_146"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_146 x2)))
term_strict_isEQ x1 = DI.Term "isEQ" (DI.SrcID "Prelude" 0) x1
 
strict_compare :: (DM.DM dm) => Int -> Int -> dm Ordering
strict_compare x1 x2
  = DM.eval
      (DM.funcDeclHook "compare"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_145"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_145 x3 x4)))
term_strict_compare x1
  = DI.Term "compare" (DI.SrcID "Prelude" 0) x1
 
op_Lt :: (DM.DM dm) => Int -> Int -> dm Bool
op_Lt x1 x2
  = DM.eval
      (DM.funcDeclHook "<"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "compare"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_compare x3 x4)
             x6 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     (Prelude.return LT)
             DM.funcCallHook "=="
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (op_EqEq x5 x6)))
term_op_Lt x1 = DI.Term "<" (DI.SrcID "Prelude" 0) x1
 
op_Gt :: (DM.DM dm) => Int -> Int -> dm Bool
op_Gt x1 x2
  = DM.eval
      (DM.funcDeclHook ">"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "compare"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_compare x3 x4)
             x6 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     (Prelude.return GT)
             DM.funcCallHook "=="
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (op_EqEq x5 x6)))
term_op_Gt x1 = DI.Term ">" (DI.SrcID "Prelude" 0) x1
 
op_LtEq :: (DM.DM dm) => Int -> Int -> dm Bool
op_LtEq x1 x2
  = DM.eval
      (DM.funcDeclHook "<="
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "compare"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_compare x3 x4)
             x6 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     (Prelude.return GT)
             DM.funcCallHook "/="
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (op_SlashEq x5 x6)))
term_op_LtEq x1 = DI.Term "<=" (DI.SrcID "Prelude" 0) x1
 
op_GtEq :: (DM.DM dm) => Int -> Int -> dm Bool
op_GtEq x1 x2
  = DM.eval
      (DM.funcDeclHook ">="
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "compare"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_compare x3 x4)
             x6 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     (Prelude.return LT)
             DM.funcCallHook "/="
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (op_SlashEq x5 x6)))
term_op_GtEq x1 = DI.Term ">=" (DI.SrcID "Prelude" 0) x1
 
strict_max :: (DM.DM dm) => Int -> Int -> dm Int
strict_max x1 x2
  = DM.eval
      (DM.funcDeclHook "max"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "compare"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_compare x3 x4)
             DM.funcCallHook "_case_141"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_141 x5 x6 x7)))
term_strict_max x1 = DI.Term "max" (DI.SrcID "Prelude" 0) x1
 
strict_min :: (DM.DM dm) => Int -> Int -> dm Int
strict_min x1 x2
  = DM.eval
      (DM.funcDeclHook "min"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "compare"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_compare x3 x4)
             DM.funcCallHook "_case_140"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_140 x5 x6 x7)))
term_strict_min x1 = DI.Term "min" (DI.SrcID "Prelude" 0) x1
 
op_SlashEq :: (DM.DM dm, DI.GenTerm a) => a -> a -> dm Bool
op_SlashEq x1 x2
  = DM.eval
      (DM.funcDeclHook "/="
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (op_EqEq x3 x4)
             DM.funcCallHook "not"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (strict_not x5)))
term_op_SlashEq x1 = DI.Term "/=" (DI.SrcID "Prelude" 0) x1
 
strict_fst ::
           (DM.DM dm, DI.GenTerm a, DI.GenTerm b) => Tuple2 a b -> dm a
strict_fst x1
  = DM.eval
      (DM.funcDeclHook "fst"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_139"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_139 x2)))
term_strict_fst x1 = DI.Term "fst" (DI.SrcID "Prelude" 0) x1
 
strict_snd ::
           (DM.DM dm, DI.GenTerm a, DI.GenTerm b) => Tuple2 a b -> dm b
strict_snd x1
  = DM.eval
      (DM.funcDeclHook "snd"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_138"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_138 x2)))
term_strict_snd x1 = DI.Term "snd" (DI.SrcID "Prelude" 0) x1
 
strict_head :: (DM.DM dm, DI.GenTerm a) => List a -> dm a
strict_head x1
  = DM.eval
      (DM.funcDeclHook "head"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_137"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_137 x2)))
term_strict_head x1 = DI.Term "head" (DI.SrcID "Prelude" 0) x1
 
strict_tail :: (DM.DM dm, DI.GenTerm a) => List a -> dm (List a)
strict_tail x1
  = DM.eval
      (DM.funcDeclHook "tail"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_136"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_136 x2)))
term_strict_tail x1 = DI.Term "tail" (DI.SrcID "Prelude" 0) x1
 
strict_null :: (DM.DM dm, DI.GenTerm a) => List a -> dm Bool
strict_null x1
  = DM.eval
      (DM.funcDeclHook "null"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_135"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_135 x2)))
term_strict_null x1 = DI.Term "null" (DI.SrcID "Prelude" 0) x1
 
op_PlusPlus ::
            (DM.DM dm, DI.GenTerm a) => List a -> List a -> dm (List a)
op_PlusPlus x1 x2
  = DM.eval
      (DM.funcDeclHook "++"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_134"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_134 x3 x4)))
term_op_PlusPlus x1 = DI.Term "++" (DI.SrcID "Prelude" 0) x1
 
strict_length :: (DM.DM dm, DI.GenTerm a) => List a -> dm Int
strict_length x1
  = DM.eval
      (DM.funcDeclHook "length"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_133"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_133 x2)))
term_strict_length x1 = DI.Term "length" (DI.SrcID "Prelude" 0) x1
 
op_EMarkEMark :: (DM.DM dm, DI.GenTerm a) => List a -> Int -> dm a
op_EMarkEMark x1 x2
  = DM.eval
      (DM.funcDeclHook "!!"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_132"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_132 x3 x4)))
term_op_EMarkEMark x1 = DI.Term "!!" (DI.SrcID "Prelude" 0) x1
 
strict_map ::
           (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
             DM.Func dm a b -> List a -> dm (List b)
strict_map x1 x2
  = DM.eval
      (DM.funcDeclHook "map"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_129"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_129 x3 x4)))
term_strict_map x1 = DI.Term "map" (DI.SrcID "Prelude" 0) x1
 
strict_foldl ::
             (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
               DM.Func dm a (DM.Func dm b a) -> a -> List b -> dm a
strict_foldl x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "foldl"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x2
             x6 <- Prelude.return x3
             DM.funcCallHook "_case_128"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_128 x4 x5 x6)))
term_strict_foldl x1 = DI.Term "foldl" (DI.SrcID "Prelude" 0) x1
 
strict_foldl1 ::
              (DM.DM dm, DI.GenTerm a) =>
                DM.Func dm a (DM.Func dm a a) -> List a -> dm a
strict_foldl1 x1 x2
  = DM.eval
      (DM.funcDeclHook "foldl1"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_127"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_127 x3 x4)))
term_strict_foldl1 x1 = DI.Term "foldl1" (DI.SrcID "Prelude" 0) x1
 
strict_foldr ::
             (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
               DM.Func dm a (DM.Func dm b b) -> b -> List a -> dm b
strict_foldr x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "foldr"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x2
             x6 <- Prelude.return x3
             DM.funcCallHook "_case_126"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_126 x4 x5 x6)))
term_strict_foldr x1 = DI.Term "foldr" (DI.SrcID "Prelude" 0) x1
 
strict_foldr1 ::
              (DM.DM dm, DI.GenTerm a) =>
                DM.Func dm a (DM.Func dm a a) -> List a -> dm a
strict_foldr1 x1 x2
  = DM.eval
      (DM.funcDeclHook "foldr1"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_125"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_125 x3 x4)))
term_strict_foldr1 x1 = DI.Term "foldr1" (DI.SrcID "Prelude" 0) x1
 
strict_filter ::
              (DM.DM dm, DI.GenTerm a) =>
                DM.Func dm a Bool -> List a -> dm (List a)
strict_filter x1 x2
  = DM.eval
      (DM.funcDeclHook "filter"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_123"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_123 x3 x4)))
term_strict_filter x1 = DI.Term "filter" (DI.SrcID "Prelude" 0) x1
 
strict_zip ::
           (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
             List a -> List b -> dm (List (Tuple2 a b))
strict_zip x1 x2
  = DM.eval
      (DM.funcDeclHook "zip"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_121"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_121 x3 x4)))
term_strict_zip x1 = DI.Term "zip" (DI.SrcID "Prelude" 0) x1
 
strict_zip3 ::
            (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) =>
              List a -> List b -> List c -> dm (List (Tuple3 a b c))
strict_zip3 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "zip3"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x2
             x5 <- Prelude.return x3
             x6 <- Prelude.return x1
             DM.funcCallHook "_case_119"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_119 x4 x5 x6)))
term_strict_zip3 x1 = DI.Term "zip3" (DI.SrcID "Prelude" 0) x1
 
strict_zipWith ::
               (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) =>
                 DM.Func dm a (DM.Func dm b c) -> List a -> List b -> dm (List c)
strict_zipWith x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "zipWith"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x3
             x6 <- Prelude.return x2
             DM.funcCallHook "_case_116"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_116 x4 x5 x6)))
term_strict_zipWith x1
  = DI.Term "zipWith" (DI.SrcID "Prelude" 0) x1
 
strict_zipWith3 ::
                (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c,
                 DI.GenTerm d) =>
                  DM.Func dm a (DM.Func dm b (DM.Func dm c d)) ->
                    List a -> List b -> List c -> dm (List d)
strict_zipWith3 x1 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "zipWith3"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x3
             x7 <- Prelude.return x4
             x8 <- Prelude.return x2
             DM.funcCallHook "_case_114"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8]))
               (strict__case_114 x5 x6 x7 x8)))
term_strict_zipWith3 x1
  = DI.Term "zipWith3" (DI.SrcID "Prelude" 0) x1
 
strict_unzip ::
             (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
               List (Tuple2 a b) -> dm (Tuple2 (List a) (List b))
strict_unzip x1
  = DM.eval
      (DM.funcDeclHook "unzip"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_111"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_111 x2)))
term_strict_unzip x1 = DI.Term "unzip" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_unzip46_35selFP335xs ::
                               (DM.DM dm, DI.GenTerm x476, DI.GenTerm x477) =>
                                 Tuple2 (List x476) (List x477) -> dm (List x476)
x'xstrict_unzip46_35selFP335xs x1
  = DM.eval
      (DM.funcDeclHook "unzip._#selFP3#xs"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_109"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_109 x2)))
x'xterm_strict_unzip46_35selFP335xs x1
  = DI.Term "unzip._#selFP3#xs" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_unzip46_35selFP435ys ::
                               (DM.DM dm, DI.GenTerm x476, DI.GenTerm x477) =>
                                 Tuple2 (List x476) (List x477) -> dm (List x477)
x'xstrict_unzip46_35selFP435ys x1
  = DM.eval
      (DM.funcDeclHook "unzip._#selFP4#ys"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_108"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_108 x2)))
x'xterm_strict_unzip46_35selFP435ys x1
  = DI.Term "unzip._#selFP4#ys" (DI.SrcID "Prelude" 0) x1
 
strict_unzip3 ::
              (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) =>
                List (Tuple3 a b c) -> dm (Tuple3 (List a) (List b) (List c))
strict_unzip3 x1
  = DM.eval
      (DM.funcDeclHook "unzip3"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_107"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_107 x2)))
term_strict_unzip3 x1 = DI.Term "unzip3" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_unzip346_35selFP635xs ::
                                (DM.DM dm, DI.GenTerm x493, DI.GenTerm x494, DI.GenTerm x495) =>
                                  Tuple3 (List x493) (List x494) (List x495) -> dm (List x493)
x'xstrict_unzip346_35selFP635xs x1
  = DM.eval
      (DM.funcDeclHook "unzip3._#selFP6#xs"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_105"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_105 x2)))
x'xterm_strict_unzip346_35selFP635xs x1
  = DI.Term "unzip3._#selFP6#xs" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_unzip346_35selFP735ys ::
                                (DM.DM dm, DI.GenTerm x493, DI.GenTerm x494, DI.GenTerm x495) =>
                                  Tuple3 (List x493) (List x494) (List x495) -> dm (List x494)
x'xstrict_unzip346_35selFP735ys x1
  = DM.eval
      (DM.funcDeclHook "unzip3._#selFP7#ys"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_104"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_104 x2)))
x'xterm_strict_unzip346_35selFP735ys x1
  = DI.Term "unzip3._#selFP7#ys" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_unzip346_35selFP835zs ::
                                (DM.DM dm, DI.GenTerm x493, DI.GenTerm x494, DI.GenTerm x495) =>
                                  Tuple3 (List x493) (List x494) (List x495) -> dm (List x495)
x'xstrict_unzip346_35selFP835zs x1
  = DM.eval
      (DM.funcDeclHook "unzip3._#selFP8#zs"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_103"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_103 x2)))
x'xterm_strict_unzip346_35selFP835zs x1
  = DI.Term "unzip3._#selFP8#zs" (DI.SrcID "Prelude" 0) x1
 
strict_concat ::
              (DM.DM dm, DI.GenTerm a) => List (List a) -> dm (List a)
strict_concat x1
  = DM.eval
      (DM.funcDeclHook "concat"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall2 (term_op_PlusPlus []) op_PlusPlus)
             x3 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Nil)
             x4 <- Prelude.return x1
             DM.funcCallHook "foldr"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
               (strict_foldr x2 x3 x4)))
term_strict_concat x1 = DI.Term "concat" (DI.SrcID "Prelude" 0) x1
 
strict_concatMap ::
                 (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                   DM.Func dm a (List b) -> dm (DM.Func dm (List a) (List b))
strict_concatMap x1
  = DM.eval
      (DM.funcDeclHook "concatMap"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return
                     (PC.partCall1 (term_strict_concat []) strict_concat)
             x4 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1 (term_strict_map [DI.genTerm x2]) (strict_map x2))
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (op_Point x3 x4)))
term_strict_concatMap x1
  = DI.Term "concatMap" (DI.SrcID "Prelude" 0) x1
 
strict_iterate ::
               (DM.DM dm, DI.GenTerm a) => DM.Func dm a a -> a -> dm (List a)
strict_iterate x1 x2
  = DM.eval
      (DM.funcDeclHook "iterate"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x7 <- Prelude.return x2
             x8 <- do x5 <- Prelude.return x1
                      x6 <- do x3 <- Prelude.return x1
                               x4 <- Prelude.return x2
                               DM.funcCallHook "apply"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                 (strict_apply x3 x4)
                      DM.funcCallHook "iterate"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                        (strict_iterate x5 x6)
             DM.constructorHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
               (Prelude.return (Cons x7 x8))))
term_strict_iterate x1
  = DI.Term "iterate" (DI.SrcID "Prelude" 0) x1
 
strict_repeat :: (DM.DM dm, DI.GenTerm a) => a -> dm (List a)
strict_repeat x1
  = DM.eval
      (DM.funcDeclHook "repeat"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "repeat"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_repeat x2)
             DM.constructorHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (Prelude.return (Cons x3 x4))))
term_strict_repeat x1 = DI.Term "repeat" (DI.SrcID "Prelude" 0) x1
 
strict_replicate ::
                 (DM.DM dm, DI.GenTerm a) => Int -> a -> dm (List a)
strict_replicate x1 x2
  = DM.eval
      (DM.funcDeclHook "replicate"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x4 <- Prelude.return x1
             x5 <- do x3 <- Prelude.return x2
                      DM.funcCallHook "repeat"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3]))
                        (strict_repeat x3)
             DM.funcCallHook "take"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (strict_take x4 x5)))
term_strict_replicate x1
  = DI.Term "replicate" (DI.SrcID "Prelude" 0) x1
 
strict_take ::
            (DM.DM dm, DI.GenTerm a) => Int -> List a -> dm (List a)
strict_take x1 x2
  = DM.eval
      (DM.funcDeclHook "take"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_102"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_102 x3 x4)))
term_strict_take x1 = DI.Term "take" (DI.SrcID "Prelude" 0) x1
 
strict_drop ::
            (DM.DM dm, DI.GenTerm a) => Int -> List a -> dm (List a)
strict_drop x1 x2
  = DM.eval
      (DM.funcDeclHook "drop"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Zero)
                      DM.funcCallHook "<="
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (op_LtEq x3 x4)
             DM.funcCallHook "_case_100"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_100 x5 x6 x7)))
term_strict_drop x1 = DI.Term "drop" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_drop46dropp46272 ::
                           (DM.DM dm, DI.GenTerm a) => Int -> List a -> dm (List a)
x'xstrict_drop46dropp46272 x1 x2
  = DM.eval
      (DM.funcDeclHook "drop.dropp.272"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_99"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_99 x3 x4)))
x'xterm_strict_drop46dropp46272 x1
  = DI.Term "drop.dropp.272" (DI.SrcID "Prelude" 0) x1
 
strict_splitAt ::
               (DM.DM dm, DI.GenTerm a) =>
                 Int -> List a -> dm (Tuple2 (List a) (List a))
strict_splitAt x1 x2
  = DM.eval
      (DM.funcDeclHook "splitAt"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Zero)
                      DM.funcCallHook "<="
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (op_LtEq x3 x4)
             DM.funcCallHook "_case_98"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_98 x5 x6 x7)))
term_strict_splitAt x1
  = DI.Term "splitAt" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_splitAt46splitAtp46282 ::
                                 (DM.DM dm, DI.GenTerm a) =>
                                   Int -> List a -> dm (Tuple2 (List a) (List a))
x'xstrict_splitAt46splitAtp46282 x1 x2
  = DM.eval
      (DM.funcDeclHook "splitAt.splitAtp.282"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_97"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_97 x3 x4)))
x'xterm_strict_splitAt46splitAtp46282 x1
  = DI.Term "splitAt.splitAtp.282" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_splitAt46splitAtp4628246_35selFP1035ys ::
                                                 (DM.DM dm, DI.GenTerm x576) =>
                                                   Tuple2 (List x576) (List x576) -> dm (List x576)
x'xstrict_splitAt46splitAtp4628246_35selFP1035ys x1
  = DM.eval
      (DM.funcDeclHook "splitAt.splitAtp.282._#selFP10#ys"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_96"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_96 x2)))
x'xterm_strict_splitAt46splitAtp4628246_35selFP1035ys x1
  = DI.Term "splitAt.splitAtp.282._#selFP10#ys"
      (DI.SrcID "Prelude" 0)
      x1
 
x'xstrict_splitAt46splitAtp4628246_35selFP1135zs ::
                                                 (DM.DM dm, DI.GenTerm x576) =>
                                                   Tuple2 (List x576) (List x576) -> dm (List x576)
x'xstrict_splitAt46splitAtp4628246_35selFP1135zs x1
  = DM.eval
      (DM.funcDeclHook "splitAt.splitAtp.282._#selFP11#zs"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_95"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_95 x2)))
x'xterm_strict_splitAt46splitAtp4628246_35selFP1135zs x1
  = DI.Term "splitAt.splitAtp.282._#selFP11#zs"
      (DI.SrcID "Prelude" 0)
      x1
 
strict_takeWhile ::
                 (DM.DM dm, DI.GenTerm a) =>
                   DM.Func dm a Bool -> List a -> dm (List a)
strict_takeWhile x1 x2
  = DM.eval
      (DM.funcDeclHook "takeWhile"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_94"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_94 x3 x4)))
term_strict_takeWhile x1
  = DI.Term "takeWhile" (DI.SrcID "Prelude" 0) x1
 
strict_dropWhile ::
                 (DM.DM dm, DI.GenTerm a) =>
                   DM.Func dm a Bool -> List a -> dm (List a)
strict_dropWhile x1 x2
  = DM.eval
      (DM.funcDeclHook "dropWhile"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_92"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_92 x3 x4)))
term_strict_dropWhile x1
  = DI.Term "dropWhile" (DI.SrcID "Prelude" 0) x1
 
strict_span ::
            (DM.DM dm, DI.GenTerm a) =>
              DM.Func dm a Bool -> List a -> dm (Tuple2 (List a) (List a))
strict_span x1 x2
  = DM.eval
      (DM.funcDeclHook "span"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_90"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_90 x3 x4)))
term_strict_span x1 = DI.Term "span" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_span46_35selFP1335ys ::
                               (DM.DM dm, DI.GenTerm x627) =>
                                 Tuple2 (List x627) (List x627) -> dm (List x627)
x'xstrict_span46_35selFP1335ys x1
  = DM.eval
      (DM.funcDeclHook "span._#selFP13#ys"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_87"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_87 x2)))
x'xterm_strict_span46_35selFP1335ys x1
  = DI.Term "span._#selFP13#ys" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_span46_35selFP1435zs ::
                               (DM.DM dm, DI.GenTerm x627) =>
                                 Tuple2 (List x627) (List x627) -> dm (List x627)
x'xstrict_span46_35selFP1435zs x1
  = DM.eval
      (DM.funcDeclHook "span._#selFP14#zs"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_86"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_86 x2)))
x'xterm_strict_span46_35selFP1435zs x1
  = DI.Term "span._#selFP14#zs" (DI.SrcID "Prelude" 0) x1
 
strict_break ::
             (DM.DM dm, DI.GenTerm a) =>
               DM.Func dm a Bool ->
                 dm (DM.Func dm (List a) (Tuple2 (List a) (List a)))
strict_break x1
  = DM.eval
      (DM.funcDeclHook "break"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return
                              (PC.partCall1 (term_strict_not []) strict_not)
                      x3 <- Prelude.return x1
                      DM.funcCallHook "."
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (op_Point x2 x3)
             Prelude.return
               (PC.partCall1 (term_strict_span [DI.genTerm x4])
                  (strict_span x4))))
term_strict_break x1 = DI.Term "break" (DI.SrcID "Prelude" 0) x1
 
strict_lines :: (DM.DM dm) => List Char -> dm (List (List Char))
strict_lines x1
  = DM.eval
      (DM.funcDeclHook "lines"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_85"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_85 x2)))
term_strict_lines x1 = DI.Term "lines" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_lines46splitline46314 ::
                                (DM.DM dm) => List Char -> dm (Tuple2 (List Char) (List Char))
x'xstrict_lines46splitline46314 x1
  = DM.eval
      (DM.funcDeclHook "lines.splitline.314"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_84"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_84 x2)))
x'xterm_strict_lines46splitline46314 x1
  = DI.Term "lines.splitline.314" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_lines46splitline4631446_35selFP1635ds ::
                                                (DM.DM dm) =>
                                                  Tuple2 (List Char) (List Char) -> dm (List Char)
x'xstrict_lines46splitline4631446_35selFP1635ds x1
  = DM.eval
      (DM.funcDeclHook "lines.splitline.314._#selFP16#ds"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_82"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_82 x2)))
x'xterm_strict_lines46splitline4631446_35selFP1635ds x1
  = DI.Term "lines.splitline.314._#selFP16#ds" (DI.SrcID "Prelude" 0)
      x1
 
x'xstrict_lines46splitline4631446_35selFP1735es ::
                                                (DM.DM dm) =>
                                                  Tuple2 (List Char) (List Char) -> dm (List Char)
x'xstrict_lines46splitline4631446_35selFP1735es x1
  = DM.eval
      (DM.funcDeclHook "lines.splitline.314._#selFP17#es"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_81"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_81 x2)))
x'xterm_strict_lines46splitline4631446_35selFP1735es x1
  = DI.Term "lines.splitline.314._#selFP17#es" (DI.SrcID "Prelude" 0)
      x1
 
x'xstrict_lines46_35selFP1935l ::
                               (DM.DM dm) => Tuple2 (List Char) (List Char) -> dm (List Char)
x'xstrict_lines46_35selFP1935l x1
  = DM.eval
      (DM.funcDeclHook "lines._#selFP19#l"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_80"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_80 x2)))
x'xterm_strict_lines46_35selFP1935l x1
  = DI.Term "lines._#selFP19#l" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_lines46_35selFP2035xs_l ::
                                  (DM.DM dm) => Tuple2 (List Char) (List Char) -> dm (List Char)
x'xstrict_lines46_35selFP2035xs_l x1
  = DM.eval
      (DM.funcDeclHook "lines._#selFP20#xs_l"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_79"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_79 x2)))
x'xterm_strict_lines46_35selFP2035xs_l x1
  = DI.Term "lines._#selFP20#xs_l" (DI.SrcID "Prelude" 0) x1
 
strict_unlines :: (DM.DM dm) => List (List Char) -> dm (List Char)
strict_unlines x1
  = DM.eval
      (DM.funcDeclHook "unlines"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x7 <- do x6 <- do x4 <- Prelude.return
                                       (PC.partCall2 (term_op_PlusPlus []) op_PlusPlus)
                               x5 <- do x2 <- DM.litHook
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo [] []))
                                                (Prelude.return (Char '\n'))
                                        x3 <- DM.constructorHook
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo [] []))
                                                (Prelude.return Nil)
                                        DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                                          (Prelude.return (Cons x2 x3))
                               Prelude.return
                                 (PC.partCall1 (term_strict_flip [DI.genTerm x4, DI.genTerm x5])
                                    (strict_flip x4 x5))
                      DM.funcCallHook "concatMap"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x6]))
                        (strict_concatMap x6)
             x8 <- Prelude.return x1
             DM.funcCallHook "apply"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
               (strict_apply x7 x8)))
term_strict_unlines x1
  = DI.Term "unlines" (DI.SrcID "Prelude" 0) x1
 
strict_words :: (DM.DM dm) => List Char -> dm (List (List Char))
strict_words x1
  = DM.eval
      (DM.funcDeclHook "words"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (DM.letHook
            (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
            (do x2 <- do x3 <- Prelude.return
                                 (PC.partCall1 (x'xterm_strict_words46isSpace46326 [])
                                    x'xstrict_words46isSpace46326)
                         x4 <- Prelude.return x1
                         DM.funcCallHook "dropWhile"
                           (DI.DebugInfo (DI.SrcID "Prelude" 0)
                              (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                           (strict_dropWhile x3 x4)
                DM.eval
                  (do x7 <- Prelude.return x2
                      x8 <- do x5 <- Prelude.return x2
                               x6 <- DM.constructorHook
                                       (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                       (Prelude.return Nil)
                               DM.funcCallHook "=="
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                 (op_EqEq x5 x6)
                      DM.funcCallHook "_case_78"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                        (strict__case_78 x7 x8)))))
term_strict_words x1 = DI.Term "words" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_words46isSpace46326 :: (DM.DM dm) => Char -> dm Bool
x'xstrict_words46isSpace46326 x1
  = DM.eval
      (DM.funcDeclHook "words.isSpace.326"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x14 <- do x2 <- Prelude.return x1
                       x3 <- DM.litHook
                               (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                               (Prelude.return (Char ' '))
                       DM.funcCallHook "=="
                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                         (op_EqEq x2 x3)
             x15 <- do x12 <- do x4 <- Prelude.return x1
                                 x5 <- DM.litHook
                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                            (DI.DynamicInfo [] []))
                                         (Prelude.return (Char '\t'))
                                 DM.funcCallHook "=="
                                   (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                   (op_EqEq x4 x5)
                       x13 <- do x10 <- do x6 <- Prelude.return x1
                                           x7 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return (Char '\n'))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (op_EqEq x6 x7)
                                 x11 <- do x8 <- Prelude.return x1
                                           x9 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return (Char '\r'))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                             (op_EqEq x8 x9)
                                 DM.funcCallHook "||"
                                   (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                   (op_OrOr x10 x11)
                       DM.funcCallHook "||"
                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                            (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                         (op_OrOr x12 x13)
             DM.funcCallHook "||"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
               (op_OrOr x14 x15)))
x'xterm_strict_words46isSpace46326 x1
  = DI.Term "words.isSpace.326" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_words46_35selFP2235w ::
                               (DM.DM dm) => Tuple2 (List Char) (List Char) -> dm (List Char)
x'xstrict_words46_35selFP2235w x1
  = DM.eval
      (DM.funcDeclHook "words._#selFP22#w"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_77"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_77 x2)))
x'xterm_strict_words46_35selFP2235w x1
  = DI.Term "words._#selFP22#w" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_words46_35selFP2335s2 ::
                                (DM.DM dm) => Tuple2 (List Char) (List Char) -> dm (List Char)
x'xstrict_words46_35selFP2335s2 x1
  = DM.eval
      (DM.funcDeclHook "words._#selFP23#s2"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_76"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_76 x2)))
x'xterm_strict_words46_35selFP2335s2 x1
  = DI.Term "words._#selFP23#s2" (DI.SrcID "Prelude" 0) x1
 
strict_unwords :: (DM.DM dm) => List (List Char) -> dm (List Char)
strict_unwords x1
  = DM.eval
      (DM.funcDeclHook "unwords"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             x5 <- do x2 <- Prelude.return x1
                      x3 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Nil)
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (op_EqEq x2 x3)
             DM.funcCallHook "_case_75"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (strict__case_75 x4 x5)))
term_strict_unwords x1
  = DI.Term "unwords" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_unwords46_35lambda6 ::
                              (DM.DM dm) => List Char -> List Char -> dm (List Char)
x'xstrict_unwords46_35lambda6 x1 x2
  = DM.eval
      (DM.funcDeclHook "unwords._#lambda6"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- do x3 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Char ' '))
                      x4 <- Prelude.return x2
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Prelude.return (Cons x3 x4))
             DM.funcCallHook "++"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (op_PlusPlus x5 x6)))
x'xterm_strict_unwords46_35lambda6 x1
  = DI.Term "unwords._#lambda6" (DI.SrcID "Prelude" 0) x1
 
strict_reverse ::
               (DM.DM dm, DI.GenTerm a) => dm (DM.Func dm (List a) (List a))
strict_reverse
  = DM.eval
      (DM.funcDeclHook "reverse"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (do x1 <- do x0 <- Prelude.return
                              (PC.partCall2 (term_Cons [])
                                 (\ x0 x1 -> Prelude.return (Cons x0 x1)))
                      Prelude.return
                        (PC.partCall2 (term_strict_flip [DI.genTerm x0]) (strict_flip x0))
             x2 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Nil)
             Prelude.return
               (PC.partCall1 (term_strict_foldl [DI.genTerm x1, DI.genTerm x2])
                  (strict_foldl x1 x2))))
 
strict_and :: (DM.DM dm) => dm (DM.Func dm (List Bool) Bool)
strict_and
  = DM.eval
      (DM.funcDeclHook "and"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return
                     (PC.partCall2 (term_op_AndAnd []) op_AndAnd)
             x1 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     (Prelude.return True)
             Prelude.return
               (PC.partCall1 (term_strict_foldr [DI.genTerm x0, DI.genTerm x1])
                  (strict_foldr x0 x1))))
 
strict_or :: (DM.DM dm) => dm (DM.Func dm (List Bool) Bool)
strict_or
  = DM.eval
      (DM.funcDeclHook "or"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return (PC.partCall2 (term_op_OrOr []) op_OrOr)
             x1 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     (Prelude.return False)
             Prelude.return
               (PC.partCall1 (term_strict_foldr [DI.genTerm x0, DI.genTerm x1])
                  (strict_foldr x0 x1))))
 
strict_any ::
           (DM.DM dm, DI.GenTerm a) =>
             DM.Func dm a Bool -> dm (DM.Func dm (List a) Bool)
strict_any x1
  = DM.eval
      (DM.funcDeclHook "any"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- DM.funcCallHook "or"
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     strict_or
             x4 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1 (term_strict_map [DI.genTerm x2]) (strict_map x2))
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (op_Point x3 x4)))
term_strict_any x1 = DI.Term "any" (DI.SrcID "Prelude" 0) x1
 
strict_all ::
           (DM.DM dm, DI.GenTerm a) =>
             DM.Func dm a Bool -> dm (DM.Func dm (List a) Bool)
strict_all x1
  = DM.eval
      (DM.funcDeclHook "all"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- DM.funcCallHook "and"
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     strict_and
             x4 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1 (term_strict_map [DI.genTerm x2]) (strict_map x2))
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (op_Point x3 x4)))
term_strict_all x1 = DI.Term "all" (DI.SrcID "Prelude" 0) x1
 
strict_elem ::
            (DM.DM dm, DI.GenTerm a) => a -> dm (DM.Func dm (List a) Bool)
strict_elem x1
  = DM.eval
      (DM.funcDeclHook "elem"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1 (term_op_EqEq [DI.genTerm x2]) (op_EqEq x2))
             DM.funcCallHook "any"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (strict_any x3)))
term_strict_elem x1 = DI.Term "elem" (DI.SrcID "Prelude" 0) x1
 
strict_notElem ::
               (DM.DM dm, DI.GenTerm a) => a -> dm (DM.Func dm (List a) Bool)
strict_notElem x1
  = DM.eval
      (DM.funcDeclHook "notElem"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1 (term_op_SlashEq [DI.genTerm x2]) (op_SlashEq x2))
             DM.funcCallHook "all"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (strict_all x3)))
term_strict_notElem x1
  = DI.Term "notElem" (DI.SrcID "Prelude" 0) x1
 
strict_lookup ::
              (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                a -> List (Tuple2 a b) -> dm (Maybe b)
strict_lookup x1 x2
  = DM.eval
      (DM.funcDeclHook "lookup"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_74"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_74 x3 x4)))
term_strict_lookup x1 = DI.Term "lookup" (DI.SrcID "Prelude" 0) x1
 
strict_enumFrom :: (DM.DM dm) => Int -> dm (List Int)
strict_enumFrom x1
  = DM.eval
      (DM.funcDeclHook "enumFrom"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x5 <- Prelude.return x1
             x6 <- do x4 <- do x2 <- Prelude.return x1
                               x3 <- DM.litHook
                                       (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                       (Prelude.return (Pos IHi))
                               DM.funcCallHook "+"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                                 (op_Plus x2 x3)
                      DM.funcCallHook "enumFrom"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4]))
                        (strict_enumFrom x4)
             DM.constructorHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Prelude.return (Cons x5 x6))))
term_strict_enumFrom x1
  = DI.Term "enumFrom" (DI.SrcID "Prelude" 0) x1
 
strict_enumFromThen :: (DM.DM dm) => Int -> Int -> dm (List Int)
strict_enumFromThen x1 x2
  = DM.eval
      (DM.funcDeclHook "enumFromThen"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x6 <- do x5 <- do x3 <- Prelude.return x2
                               x4 <- Prelude.return x1
                               DM.funcCallHook "-"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                 (op_Minus x3 x4)
                      Prelude.return
                        (PC.partCall1 (term_op_Plus [DI.genTerm x5]) (op_Plus x5))
             x7 <- Prelude.return x1
             DM.funcCallHook "iterate"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
               (strict_iterate x6 x7)))
term_strict_enumFromThen x1
  = DI.Term "enumFromThen" (DI.SrcID "Prelude" 0) x1
 
strict_enumFromTo :: (DM.DM dm) => Int -> Int -> dm (List Int)
strict_enumFromTo x1 x2
  = DM.eval
      (DM.funcDeclHook "enumFromTo"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook ">"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (op_Gt x3 x4)
             DM.funcCallHook "_case_70"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_70 x5 x6 x7)))
term_strict_enumFromTo x1
  = DI.Term "enumFromTo" (DI.SrcID "Prelude" 0) x1
 
strict_enumFromThenTo ::
                      (DM.DM dm) => Int -> Int -> Int -> dm (List Int)
strict_enumFromThenTo x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "enumFromThenTo"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x9 <- do x4 <- Prelude.return x3
                      x5 <- Prelude.return x1
                      x6 <- Prelude.return x2
                      Prelude.return
                        (PC.partCall1
                           (x'xterm_strict_enumFromThenTo46p46364
                              [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6])
                           (x'xstrict_enumFromThenTo46p46364 x4 x5 x6))
             x10 <- do x7 <- Prelude.return x1
                       x8 <- Prelude.return x2
                       DM.funcCallHook "enumFromThen"
                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                            (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                         (strict_enumFromThen x7 x8)
             DM.funcCallHook "takeWhile"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
               (strict_takeWhile x9 x10)))
term_strict_enumFromThenTo x1
  = DI.Term "enumFromThenTo" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_enumFromThenTo46p46364 ::
                                 (DM.DM dm) => Int -> Int -> Int -> Int -> dm Bool
x'xstrict_enumFromThenTo46p46364 x1 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "enumFromThenTo.p.364"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x7 <- Prelude.return x1
             x8 <- Prelude.return x2
             x9 <- Prelude.return x3
             x10 <- Prelude.return x4
             x11 <- do x5 <- Prelude.return x3
                       x6 <- Prelude.return x2
                       DM.funcCallHook ">="
                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                            (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                         (op_GtEq x5 x6)
             DM.funcCallHook "_case_69"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9, DI.genTerm x10,
                      DI.genTerm x11]))
               (strict__case_69 x7 x8 x9 x10 x11)))
x'xterm_strict_enumFromThenTo46p46364 x1
  = DI.Term "enumFromThenTo.p.364" (DI.SrcID "Prelude" 0) x1
 
strict_ord :: (DM.DM dm) => Char -> dm Int
strict_ord x1
  = DM.eval
      (DM.funcDeclHook "ord"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_ord []) strict_prim_ord)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (op_DollarRhombRhomb x2 x3)))
term_strict_ord x1 = DI.Term "ord" (DI.SrcID "Prelude" 0) x1
 
strict_chr :: (DM.DM dm) => Int -> dm Char
strict_chr x1
  = DM.eval
      (DM.funcDeclHook "chr"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_chr []) strict_prim_chr)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (op_DollarRhombRhomb x2 x3)))
term_strict_chr x1 = DI.Term "chr" (DI.SrcID "Prelude" 0) x1
 
strict_succ :: (DM.DM dm) => Nat -> dm Nat
strict_succ x1
  = DM.eval
      (DM.funcDeclHook "succ"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_67"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_67 x2)))
term_strict_succ x1 = DI.Term "succ" (DI.SrcID "Prelude" 0) x1
 
op_PlusAccent :: (DM.DM dm) => Nat -> Nat -> dm Nat
op_PlusAccent x1 x2
  = DM.eval
      (DM.funcDeclHook "+^"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_66"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_66 x3 x4)))
term_op_PlusAccent x1 = DI.Term "+^" (DI.SrcID "Prelude" 0) x1
 
strict_cmpNat :: (DM.DM dm) => Nat -> Nat -> dm Ordering
strict_cmpNat x1 x2
  = DM.eval
      (DM.funcDeclHook "cmpNat"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_63"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_63 x3 x4)))
term_strict_cmpNat x1 = DI.Term "cmpNat" (DI.SrcID "Prelude" 0) x1
 
strict_cmpNatLT :: (DM.DM dm) => Nat -> Nat -> dm Ordering
strict_cmpNatLT x1 x2
  = DM.eval
      (DM.funcDeclHook "cmpNatLT"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_59"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_59 x3 x4)))
term_strict_cmpNatLT x1
  = DI.Term "cmpNatLT" (DI.SrcID "Prelude" 0) x1
 
strict_cmpNatGT :: (DM.DM dm) => Nat -> Nat -> dm Ordering
strict_cmpNatGT x1 x2
  = DM.eval
      (DM.funcDeclHook "cmpNatGT"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_56"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_56 x3 x4)))
term_strict_cmpNatGT x1
  = DI.Term "cmpNatGT" (DI.SrcID "Prelude" 0) x1
 
op_LtAccent :: (DM.DM dm) => Nat -> Nat -> dm Bool
op_LtAccent x1 x2
  = DM.eval
      (DM.funcDeclHook "<^"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "cmpNat"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_cmpNat x3 x4)
             DM.funcCallHook "isLT"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (strict_isLT x5)))
term_op_LtAccent x1 = DI.Term "<^" (DI.SrcID "Prelude" 0) x1
 
op_GtAccent :: (DM.DM dm) => Nat -> Nat -> dm Bool
op_GtAccent x1 x2
  = DM.eval
      (DM.funcDeclHook ">^"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "cmpNat"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_cmpNat x3 x4)
             DM.funcCallHook "isGT"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (strict_isGT x5)))
term_op_GtAccent x1 = DI.Term ">^" (DI.SrcID "Prelude" 0) x1
 
op_LtEqAccent :: (DM.DM dm) => Nat -> Nat -> dm Bool
op_LtEqAccent x1 x2
  = DM.eval
      (DM.funcDeclHook "<=^"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x6 <- do x5 <- do x3 <- Prelude.return x1
                               x4 <- Prelude.return x2
                               DM.funcCallHook "cmpNat"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                 (strict_cmpNat x3 x4)
                      DM.funcCallHook "isGT"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x5]))
                        (strict_isGT x5)
             DM.funcCallHook "not"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (strict_not x6)))
term_op_LtEqAccent x1 = DI.Term "<=^" (DI.SrcID "Prelude" 0) x1
 
op_GtEqAccent :: (DM.DM dm) => Nat -> Nat -> dm Bool
op_GtEqAccent x1 x2
  = DM.eval
      (DM.funcDeclHook ">=^"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x6 <- do x5 <- do x3 <- Prelude.return x1
                               x4 <- Prelude.return x2
                               DM.funcCallHook "cmpNat"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                 (strict_cmpNat x3 x4)
                      DM.funcCallHook "isLT"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x5]))
                        (strict_isLT x5)
             DM.funcCallHook "not"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (strict_not x6)))
term_op_GtEqAccent x1 = DI.Term ">=^" (DI.SrcID "Prelude" 0) x1
 
op_AsteriskAccent :: (DM.DM dm) => Nat -> Nat -> dm Nat
op_AsteriskAccent x1 x2
  = DM.eval
      (DM.funcDeclHook "*^"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_53"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_53 x3 x4)))
term_op_AsteriskAccent x1 = DI.Term "*^" (DI.SrcID "Prelude" 0) x1
 
strict_pred :: (DM.DM dm) => Nat -> dm Nat
strict_pred x1
  = DM.eval
      (DM.funcDeclHook "pred"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_52"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_52 x2)))
term_strict_pred x1 = DI.Term "pred" (DI.SrcID "Prelude" 0) x1
 
strict_inc :: (DM.DM dm) => Int -> dm Int
strict_inc x1
  = DM.eval
      (DM.funcDeclHook "inc"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_50"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_50 x2)))
term_strict_inc x1 = DI.Term "inc" (DI.SrcID "Prelude" 0) x1
 
strict_dec :: (DM.DM dm) => Int -> dm Int
strict_dec x1
  = DM.eval
      (DM.funcDeclHook "dec"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_48"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_48 x2)))
term_strict_dec x1 = DI.Term "dec" (DI.SrcID "Prelude" 0) x1
 
strict_mult2 :: (DM.DM dm) => Int -> dm Int
strict_mult2 x1
  = DM.eval
      (DM.funcDeclHook "mult2"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_46"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_46 x2)))
term_strict_mult2 x1 = DI.Term "mult2" (DI.SrcID "Prelude" 0) x1
 
op_MinusAccent :: (DM.DM dm) => Nat -> Nat -> dm Int
op_MinusAccent x1 x2
  = DM.eval
      (DM.funcDeclHook "-^"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_45"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_45 x3 x4)))
term_op_MinusAccent x1 = DI.Term "-^" (DI.SrcID "Prelude" 0) x1
 
strict_div2 :: (DM.DM dm) => Nat -> dm Nat
strict_div2 x1
  = DM.eval
      (DM.funcDeclHook "div2"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_42"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_42 x2)))
term_strict_div2 x1 = DI.Term "div2" (DI.SrcID "Prelude" 0) x1
 
strict_mod2 :: (DM.DM dm) => Nat -> dm Int
strict_mod2 x1
  = DM.eval
      (DM.funcDeclHook "mod2"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_41"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_41 x2)))
term_strict_mod2 x1 = DI.Term "mod2" (DI.SrcID "Prelude" 0) x1
 
strict_divmodNat :: (DM.DM dm) => Nat -> Nat -> dm (Tuple2 Int Int)
strict_divmodNat x1 x2
  = DM.eval
      (DM.funcDeclHook "divmodNat"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x2
                      x4 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                              (Prelude.return IHi)
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (op_EqEq x3 x4)
             DM.funcCallHook "_case_40"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_40 x5 x6 x7)))
term_strict_divmodNat x1
  = DI.Term "divmodNat" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_divmodNat46shift46523 ::
                                (DM.DM dm) => Nat -> Nat -> dm Nat
x'xstrict_divmodNat46shift46523 x1 x2
  = DM.eval
      (DM.funcDeclHook "divmodNat.shift.523"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_32"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_32 x3 x4)))
x'xterm_strict_divmodNat46shift46523 x1
  = DI.Term "divmodNat.shift.523" (DI.SrcID "Prelude" 0) x1
 
op_Plus :: (DM.DM dm) => Int -> Int -> dm Int
op_Plus x1 x2
  = DM.eval
      (DM.funcDeclHook "+"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_31"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_31 x3 x4)))
term_op_Plus x1 = DI.Term "+" (DI.SrcID "Prelude" 0) x1
 
op_Minus :: (DM.DM dm) => Int -> Int -> dm Int
op_Minus x1 x2
  = DM.eval
      (DM.funcDeclHook "-"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_28"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_28 x3 x4)))
term_op_Minus x1 = DI.Term "-" (DI.SrcID "Prelude" 0) x1
 
op_Asterisk :: (DM.DM dm) => Int -> Int -> dm Int
op_Asterisk x1 x2
  = DM.eval
      (DM.funcDeclHook "*"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_27"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_27 x3 x4)))
term_op_Asterisk x1 = DI.Term "*" (DI.SrcID "Prelude" 0) x1
 
strict_divmod :: (DM.DM dm) => Int -> Int -> dm (Tuple2 Int Int)
strict_divmod x1 x2
  = DM.eval
      (DM.funcDeclHook "divmod"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_24"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_24 x3 x4)))
term_strict_divmod x1 = DI.Term "divmod" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_divmod46_35selFP2535d ::
                                (DM.DM dm) => Tuple2 Int Int -> dm Int
x'xstrict_divmod46_35selFP2535d x1
  = DM.eval
      (DM.funcDeclHook "divmod._#selFP25#d"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_21"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_21 x2)))
x'xterm_strict_divmod46_35selFP2535d x1
  = DI.Term "divmod._#selFP25#d" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_divmod46_35selFP2635m ::
                                (DM.DM dm) => Tuple2 Int Int -> dm Int
x'xstrict_divmod46_35selFP2635m x1
  = DM.eval
      (DM.funcDeclHook "divmod._#selFP26#m"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_20"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_20 x2)))
x'xterm_strict_divmod46_35selFP2635m x1
  = DI.Term "divmod._#selFP26#m" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_divmod46_35selFP2835d ::
                                (DM.DM dm) => Tuple2 Int Int -> dm Int
x'xstrict_divmod46_35selFP2835d x1
  = DM.eval
      (DM.funcDeclHook "divmod._#selFP28#d"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_19"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_19 x2)))
x'xterm_strict_divmod46_35selFP2835d x1
  = DI.Term "divmod._#selFP28#d" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_divmod46_35selFP2935m ::
                                (DM.DM dm) => Tuple2 Int Int -> dm Int
x'xstrict_divmod46_35selFP2935m x1
  = DM.eval
      (DM.funcDeclHook "divmod._#selFP29#m"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_18"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_18 x2)))
x'xterm_strict_divmod46_35selFP2935m x1
  = DI.Term "divmod._#selFP29#m" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_divmod46_35selFP3135d ::
                                (DM.DM dm) => Tuple2 Int Int -> dm Int
x'xstrict_divmod46_35selFP3135d x1
  = DM.eval
      (DM.funcDeclHook "divmod._#selFP31#d"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_17"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_17 x2)))
x'xterm_strict_divmod46_35selFP3135d x1
  = DI.Term "divmod._#selFP31#d" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_divmod46_35selFP3235m ::
                                (DM.DM dm) => Tuple2 Int Int -> dm Int
x'xstrict_divmod46_35selFP3235m x1
  = DM.eval
      (DM.funcDeclHook "divmod._#selFP32#m"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_16"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_16 x2)))
x'xterm_strict_divmod46_35selFP3235m x1
  = DI.Term "divmod._#selFP32#m" (DI.SrcID "Prelude" 0) x1
 
strict_div :: (DM.DM dm) => Int -> Int -> dm Int
strict_div x1 x2
  = DM.eval
      (DM.funcDeclHook "div"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "divmod"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_divmod x3 x4)
             DM.funcCallHook "fst"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (strict_fst x5)))
term_strict_div x1 = DI.Term "div" (DI.SrcID "Prelude" 0) x1
 
strict_mod :: (DM.DM dm) => Int -> Int -> dm Int
strict_mod x1 x2
  = DM.eval
      (DM.funcDeclHook "mod"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "divmod"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_divmod x3 x4)
             DM.funcCallHook "snd"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (strict_snd x5)))
term_strict_mod x1 = DI.Term "mod" (DI.SrcID "Prelude" 0) x1
 
strict_negate :: (DM.DM dm) => Int -> dm Int
strict_negate x1
  = DM.eval
      (DM.funcDeclHook "negate"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_15"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_15 x2)))
term_strict_negate x1 = DI.Term "negate" (DI.SrcID "Prelude" 0) x1
 
strict_success :: (DM.DM dm) => dm Success
strict_success
  = DM.eval
      (DM.funcDeclHook "success"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (DM.constructorHook
            (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
            (Prelude.return Success)))
 
op_EqColonEq :: (DM.DM dm, DI.GenTerm a) => a -> a -> dm Success
op_EqColonEq x1 x2
  = DM.eval
      (DM.funcDeclHook "=:="
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "==="
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (op_EqEqEq x3 x4)
             DM.funcCallHook "_case_14"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_14 x5 x6 x7)))
term_op_EqColonEq x1 = DI.Term "=:=" (DI.SrcID "Prelude" 0) x1
 
op_AndGt :: (DM.DM dm, DI.GenTerm a) => Success -> a -> dm a
op_AndGt x1 x2
  = DM.eval
      (DM.funcDeclHook "&>"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "cond"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict_cond x3 x4)))
term_op_AndGt x1 = DI.Term "&>" (DI.SrcID "Prelude" 0) x1
 
strict_maybe ::
             (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
               a -> DM.Func dm b a -> Maybe b -> dm a
strict_maybe x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "maybe"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x2
             x6 <- Prelude.return x3
             DM.funcCallHook "_case_13"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_13 x4 x5 x6)))
term_strict_maybe x1 = DI.Term "maybe" (DI.SrcID "Prelude" 0) x1
 
strict_either ::
              (DM.DM dm, DI.GenTerm a, DI.GenTerm b, DI.GenTerm c) =>
                DM.Func dm a b -> DM.Func dm c b -> Either a c -> dm b
strict_either x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "either"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x2
             x6 <- Prelude.return x3
             DM.funcCallHook "_case_12"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_12 x4 x5 x6)))
term_strict_either x1 = DI.Term "either" (DI.SrcID "Prelude" 0) x1
 
op_GtGt ::
        (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
          IO dm a -> IO dm b -> dm (IO dm b)
op_GtGt x1 x2
  = DM.eval
      (DM.funcDeclHook ">>"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x4 <- Prelude.return x1
             x5 <- do x3 <- Prelude.return x2
                      Prelude.return
                        (PC.partCall1 (term_strict_const [DI.genTerm x3])
                           (strict_const x3))
             DM.funcCallHook ">>="
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (op_GtGtEq x4 x5)))
term_op_GtGt x1 = DI.Term ">>" (DI.SrcID "Prelude" 0) x1
 
strict_done :: (DM.DM dm) => dm (IO dm Unit)
strict_done
  = DM.eval
      (DM.funcDeclHook "done"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Unit)
             DM.funcCallHook "return"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x0]))
               (strict_return x0)))
 
strict_putChar :: (DM.DM dm) => Char -> dm (IO dm Unit)
strict_putChar x1
  = DM.eval
      (DM.funcDeclHook "putChar"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_putChar []) strict_prim_putChar)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (op_DollarRhombRhomb x2 x3)))
term_strict_putChar x1
  = DI.Term "putChar" (DI.SrcID "Prelude" 0) x1
 
strict_readFile ::
                (DM.DM dm) => List Char -> dm (IO dm (List Char))
strict_readFile x1
  = DM.eval
      (DM.funcDeclHook "readFile"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_readFile []) strict_prim_readFile)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (op_DollarRhombRhomb x2 x3)))
term_strict_readFile x1
  = DI.Term "readFile" (DI.SrcID "Prelude" 0) x1
 
strict_writeFile ::
                 (DM.DM dm) => List Char -> List Char -> dm (IO dm Unit)
strict_writeFile x1 x2
  = DM.eval
      (DM.funcDeclHook "writeFile"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_writeFile [])
                                 strict_prim_writeFile)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$##"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (op_DollarRhombRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (op_DollarRhombRhomb x5 x6)))
term_strict_writeFile x1
  = DI.Term "writeFile" (DI.SrcID "Prelude" 0) x1
 
strict_appendFile ::
                  (DM.DM dm) => List Char -> List Char -> dm (IO dm Unit)
strict_appendFile x1 x2
  = DM.eval
      (DM.funcDeclHook "appendFile"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_appendFile [])
                                 strict_prim_appendFile)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$##"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (op_DollarRhombRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (op_DollarRhombRhomb x5 x6)))
term_strict_appendFile x1
  = DI.Term "appendFile" (DI.SrcID "Prelude" 0) x1
 
strict_putStr :: (DM.DM dm) => List Char -> dm (IO dm Unit)
strict_putStr x1
  = DM.eval
      (DM.funcDeclHook "putStr"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_11"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_11 x2)))
term_strict_putStr x1 = DI.Term "putStr" (DI.SrcID "Prelude" 0) x1
 
strict_putStrLn :: (DM.DM dm) => List Char -> dm (IO dm Unit)
strict_putStrLn x1
  = DM.eval
      (DM.funcDeclHook "putStrLn"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "putStr"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_putStr x2)
             x5 <- do x3 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Char '\n'))
                      DM.funcCallHook "putChar"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3]))
                        (strict_putChar x3)
             DM.funcCallHook ">>"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (op_GtGt x4 x5)))
term_strict_putStrLn x1
  = DI.Term "putStrLn" (DI.SrcID "Prelude" 0) x1
 
strict_getLine :: (DM.DM dm) => dm (IO dm (List Char))
strict_getLine
  = DM.eval
      (DM.funcDeclHook "getLine"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.funcCallHook "getChar"
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     strict_getChar
             x1 <- Prelude.return
                     (PC.partCall1 (x'xterm_strict_getLine46_35lambda10 [])
                        x'xstrict_getLine46_35lambda10)
             DM.funcCallHook ">>="
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
               (op_GtGtEq x0 x1)))
 
x'xstrict_getLine46_35lambda10 ::
                               (DM.DM dm) => Char -> dm (IO dm (List Char))
x'xstrict_getLine46_35lambda10 x1
  = DM.eval
      (DM.funcDeclHook "getLine._#lambda10"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             x5 <- do x2 <- Prelude.return x1
                      x3 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Char '\n'))
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (op_EqEq x2 x3)
             DM.funcCallHook "_case_10"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (strict__case_10 x4 x5)))
x'xterm_strict_getLine46_35lambda10 x1
  = DI.Term "getLine._#lambda10" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_getLine46_35lambda1046_35lambda11 ::
                                            (DM.DM dm) =>
                                              Char -> List Char -> dm (IO dm (List Char))
x'xstrict_getLine46_35lambda1046_35lambda11 x1 x2
  = DM.eval
      (DM.funcDeclHook "getLine._#lambda10._#lambda11"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Prelude.return (Cons x3 x4))
             DM.funcCallHook "return"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (strict_return x5)))
x'xterm_strict_getLine46_35lambda1046_35lambda11 x1
  = DI.Term "getLine._#lambda10._#lambda11" (DI.SrcID "Prelude" 0) x1
 
strict_show :: (DM.DM dm, DI.GenTerm a) => a -> dm (List Char)
strict_show x1
  = DM.eval
      (DM.funcDeclHook "show"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_show []) strict_prim_show)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (op_DollarRhombRhomb x2 x3)))
term_strict_show x1 = DI.Term "show" (DI.SrcID "Prelude" 0) x1
 
strict_print :: (DM.DM dm, DI.GenTerm a) => a -> dm (IO dm Unit)
strict_print x1
  = DM.eval
      (DM.funcDeclHook "print"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "show"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_show x2)
             DM.funcCallHook "putStrLn"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (strict_putStrLn x3)))
term_strict_print x1 = DI.Term "print" (DI.SrcID "Prelude" 0) x1
 
strict_doSolve :: (DM.DM dm) => Success -> dm (IO dm Unit)
strict_doSolve x1
  = DM.eval
      (DM.funcDeclHook "doSolve"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             x3 <- DM.funcCallHook "done"
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     strict_done
             DM.funcCallHook "cond"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (strict_cond x2 x3)))
term_strict_doSolve x1
  = DI.Term "doSolve" (DI.SrcID "Prelude" 0) x1
 
strict_sequenceIO ::
                  (DM.DM dm, DI.GenTerm a) => List (IO dm a) -> dm (IO dm (List a))
strict_sequenceIO x1
  = DM.eval
      (DM.funcDeclHook "sequenceIO"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_9"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_9 x2)))
term_strict_sequenceIO x1
  = DI.Term "sequenceIO" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_sequenceIO46_35lambda12 ::
                                  (DM.DM dm, DI.GenTerm x937) =>
                                    List (IO dm x937) -> x937 -> dm (IO dm (List x937))
x'xstrict_sequenceIO46_35lambda12 x1 x2
  = DM.eval
      (DM.funcDeclHook "sequenceIO._#lambda12"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      DM.funcCallHook "sequenceIO"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3]))
                        (strict_sequenceIO x3)
             x6 <- do x4 <- Prelude.return x2
                      Prelude.return
                        (PC.partCall1
                           (x'xterm_strict_sequenceIO46_35lambda1246_35lambda13
                              [DI.genTerm x4])
                           (x'xstrict_sequenceIO46_35lambda1246_35lambda13 x4))
             DM.funcCallHook ">>="
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (op_GtGtEq x5 x6)))
x'xterm_strict_sequenceIO46_35lambda12 x1
  = DI.Term "sequenceIO._#lambda12" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_sequenceIO46_35lambda1246_35lambda13 ::
                                               (DM.DM dm, DI.GenTerm x937) =>
                                                 x937 -> List x937 -> dm (IO dm (List x937))
x'xstrict_sequenceIO46_35lambda1246_35lambda13 x1 x2
  = DM.eval
      (DM.funcDeclHook "sequenceIO._#lambda12._#lambda13"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Prelude.return (Cons x3 x4))
             DM.funcCallHook "return"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (strict_return x5)))
x'xterm_strict_sequenceIO46_35lambda1246_35lambda13 x1
  = DI.Term "sequenceIO._#lambda12._#lambda13" (DI.SrcID "Prelude" 0)
      x1
 
strict_sequenceIO_ ::
                   (DM.DM dm, DI.GenTerm a) =>
                     dm (DM.Func dm (List (IO dm a)) (IO dm Unit))
strict_sequenceIO_
  = DM.eval
      (DM.funcDeclHook "sequenceIO_"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return (PC.partCall2 (term_op_GtGt []) op_GtGt)
             x1 <- DM.funcCallHook "done"
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     strict_done
             Prelude.return
               (PC.partCall1 (term_strict_foldr [DI.genTerm x0, DI.genTerm x1])
                  (strict_foldr x0 x1))))
 
strict_mapIO ::
             (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
               DM.Func dm a (IO dm b) -> dm (DM.Func dm (List a) (IO dm (List b)))
strict_mapIO x1
  = DM.eval
      (DM.funcDeclHook "mapIO"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return
                     (PC.partCall1 (term_strict_sequenceIO []) strict_sequenceIO)
             x4 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1 (term_strict_map [DI.genTerm x2]) (strict_map x2))
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (op_Point x3 x4)))
term_strict_mapIO x1 = DI.Term "mapIO" (DI.SrcID "Prelude" 0) x1
 
strict_mapIO_ ::
              (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                DM.Func dm a (IO dm b) -> dm (DM.Func dm (List a) (IO dm Unit))
strict_mapIO_ x1
  = DM.eval
      (DM.funcDeclHook "mapIO_"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- DM.funcCallHook "sequenceIO_"
                     (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                     strict_sequenceIO_
             x4 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1 (term_strict_map [DI.genTerm x2]) (strict_map x2))
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (op_Point x3 x4)))
term_strict_mapIO_ x1 = DI.Term "mapIO_" (DI.SrcID "Prelude" 0) x1
 
op_QMark :: (DM.DM dm, DI.GenTerm a) => a -> a -> dm a
op_QMark x1 x2
  = DM.eval
      (DM.funcDeclHook "?"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (DM.orHook
            (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
            (do x3 <- Prelude.return x1
                x4 <- Prelude.return x2
                x3 ? x4)))
term_op_QMark x1 = DI.Term "?" (DI.SrcID "Prelude" 0) x1
 
strict_allValuesD ::
                  (DM.DM dm, DI.GenTerm a) => SearchTree a -> dm (List a)
strict_allValuesD x1
  = DM.eval
      (DM.funcDeclHook "allValuesD"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_8"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_8 x2)))
term_strict_allValuesD x1
  = DI.Term "allValuesD" (DI.SrcID "Prelude" 0) x1
 
strict_allValuesB ::
                  (DM.DM dm, DI.GenTerm a) => SearchTree a -> dm (List a)
strict_allValuesB x1
  = DM.eval
      (DM.funcDeclHook "allValuesB"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return x1
                      x3 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Prelude.return (Cons x2 x3))
             DM.funcCallHook "allValuesB.unfoldOrs.692"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (x'xstrict_allValuesB46unfoldOrs46692 x4)))
term_strict_allValuesB x1
  = DI.Term "allValuesB" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_allValuesB46partition46692 ::
                                     (DM.DM dm, DI.GenTerm a) =>
                                       SearchTree a ->
                                         Tuple2 (List a) (List (SearchTree a)) ->
                                           dm (Tuple2 (List a) (List (SearchTree a)))
x'xstrict_allValuesB46partition46692 x1 x2
  = DM.eval
      (DM.funcDeclHook "allValuesB.partition.692"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_7"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_7 x3 x4)))
x'xterm_strict_allValuesB46partition46692 x1
  = DI.Term "allValuesB.partition.692" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_allValuesB46partition4669246_35selFP3435vs ::
                                                     (DM.DM dm, DI.GenTerm x1001) =>
                                                       Tuple2 (List x1001) (List (SearchTree x1001))
                                                         -> dm (List x1001)
x'xstrict_allValuesB46partition4669246_35selFP3435vs x1
  = DM.eval
      (DM.funcDeclHook "allValuesB.partition.692._#selFP34#vs"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_6"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_6 x2)))
x'xterm_strict_allValuesB46partition4669246_35selFP3435vs x1
  = DI.Term "allValuesB.partition.692._#selFP34#vs"
      (DI.SrcID "Prelude" 0)
      x1
 
x'xstrict_allValuesB46partition4669246_35selFP3535ors ::
                                                      (DM.DM dm, DI.GenTerm x1001) =>
                                                        Tuple2 (List x1001)
                                                          (List (SearchTree x1001))
                                                          -> dm (List (SearchTree x1001))
x'xstrict_allValuesB46partition4669246_35selFP3535ors x1
  = DM.eval
      (DM.funcDeclHook "allValuesB.partition.692._#selFP35#ors"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_5"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_5 x2)))
x'xterm_strict_allValuesB46partition4669246_35selFP3535ors x1
  = DI.Term "allValuesB.partition.692._#selFP35#ors"
      (DI.SrcID "Prelude" 0)
      x1
 
x'xstrict_allValuesB46partition4669246_35selFP3735vs ::
                                                     (DM.DM dm, DI.GenTerm x1001) =>
                                                       Tuple2 (List x1001) (List (SearchTree x1001))
                                                         -> dm (List x1001)
x'xstrict_allValuesB46partition4669246_35selFP3735vs x1
  = DM.eval
      (DM.funcDeclHook "allValuesB.partition.692._#selFP37#vs"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_4"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_4 x2)))
x'xterm_strict_allValuesB46partition4669246_35selFP3735vs x1
  = DI.Term "allValuesB.partition.692._#selFP37#vs"
      (DI.SrcID "Prelude" 0)
      x1
 
x'xstrict_allValuesB46partition4669246_35selFP3835ors ::
                                                      (DM.DM dm, DI.GenTerm x1001) =>
                                                        Tuple2 (List x1001)
                                                          (List (SearchTree x1001))
                                                          -> dm (List (SearchTree x1001))
x'xstrict_allValuesB46partition4669246_35selFP3835ors x1
  = DM.eval
      (DM.funcDeclHook "allValuesB.partition.692._#selFP38#ors"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_3"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_3 x2)))
x'xterm_strict_allValuesB46partition4669246_35selFP3835ors x1
  = DI.Term "allValuesB.partition.692._#selFP38#ors"
      (DI.SrcID "Prelude" 0)
      x1
 
x'xstrict_allValuesB46unfoldOrs46692 ::
                                     (DM.DM dm, DI.GenTerm a) => List (SearchTree a) -> dm (List a)
x'xstrict_allValuesB46unfoldOrs46692 x1
  = DM.eval
      (DM.funcDeclHook "allValuesB.unfoldOrs.692"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_2"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_2 x2)))
x'xterm_strict_allValuesB46unfoldOrs46692 x1
  = DI.Term "allValuesB.unfoldOrs.692" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_allValuesB46unfoldOrs4669246_35selFP4035vals ::
                                                       (DM.DM dm, DI.GenTerm x1014) =>
                                                         Tuple2 (List x1014)
                                                           (List (SearchTree x1014))
                                                           -> dm (List x1014)
x'xstrict_allValuesB46unfoldOrs4669246_35selFP4035vals x1
  = DM.eval
      (DM.funcDeclHook "allValuesB.unfoldOrs.692._#selFP40#vals"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_1"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_1 x2)))
x'xterm_strict_allValuesB46unfoldOrs4669246_35selFP4035vals x1
  = DI.Term "allValuesB.unfoldOrs.692._#selFP40#vals"
      (DI.SrcID "Prelude" 0)
      x1
 
x'xstrict_allValuesB46unfoldOrs4669246_35selFP4135ors ::
                                                      (DM.DM dm, DI.GenTerm x1014) =>
                                                        Tuple2 (List x1014)
                                                          (List (SearchTree x1014))
                                                          -> dm (List (SearchTree x1014))
x'xstrict_allValuesB46unfoldOrs4669246_35selFP4135ors x1
  = DM.eval
      (DM.funcDeclHook "allValuesB.unfoldOrs.692._#selFP41#ors"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_0"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_0 x2)))
x'xterm_strict_allValuesB46unfoldOrs4669246_35selFP4135ors x1
  = DI.Term "allValuesB.unfoldOrs.692._#selFP41#ors"
      (DI.SrcID "Prelude" 0)
      x1
 
strict_inject ::
              (DM.DM dm, DI.GenTerm a) =>
                DM.Func dm a Success ->
                  DM.Func dm a Success -> dm (DM.Func dm a Success)
strict_inject x1 x2
  = DM.eval
      (DM.funcDeclHook "inject"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             Prelude.return
               (PC.partCall1
                  (x'xterm_strict_inject46_35lambda14 [DI.genTerm x3, DI.genTerm x4])
                  (x'xstrict_inject46_35lambda14 x3 x4))))
term_strict_inject x1 = DI.Term "inject" (DI.SrcID "Prelude" 0) x1
 
x'xstrict_inject46_35lambda14 ::
                              (DM.DM dm, DI.GenTerm x1025) =>
                                DM.Func dm x1025 Success ->
                                  DM.Func dm x1025 Success -> x1025 -> dm Success
x'xstrict_inject46_35lambda14 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "inject._#lambda14"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x8 <- do x4 <- Prelude.return x2
                      x5 <- Prelude.return x3
                      DM.funcCallHook "apply"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (strict_apply x4 x5)
             x9 <- do x6 <- Prelude.return x1
                      x7 <- Prelude.return x3
                      DM.funcCallHook "apply"
                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                           (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                        (strict_apply x6 x7)
             DM.funcCallHook "&"
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
               (op_And x8 x9)))
x'xterm_strict_inject46_35lambda14 x1
  = DI.Term "inject._#lambda14" (DI.SrcID "Prelude" 0) x1
 
strict_PEVAL :: (DM.DM dm, DI.GenTerm a) => a -> dm a
strict_PEVAL x1
  = DM.eval
      (DM.funcDeclHook "PEVAL"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (Prelude.return x1))
term_strict_PEVAL x1 = DI.Term "PEVAL" (DI.SrcID "Prelude" 0) x1
 
strict_unknown :: (DM.DM dm, DI.GenTerm a) => dm a
strict_unknown
  = DM.eval
      (DM.funcDeclHook "unknown"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         (DM.freeHook
            (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
            (Prelude.error "free not implemented yet")))
strict__case_0 x1
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_0
                           x4)))
term_strict__case_0 x1
  = DI.Term "_case_0" (DI.SrcID "Prelude" 0) x1
strict__case_1 x1
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_1
                           x4)))
term_strict__case_1 x1
  = DI.Term "_case_1" (DI.SrcID "Prelude" 0) x1
strict__case_2 x1
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x19 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x19]))
               (case x19 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x4 <- do x11 <- Prelude.return
                                                       (PC.partCall2
                                                          (x'xterm_strict_allValuesB46partition46692
                                                             [])
                                                          x'xstrict_allValuesB46partition46692)
                                              x12 <- do x7 <- DM.constructorHook
                                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                   (DI.DynamicInfo [] []))
                                                                (Prelude.return Nil)
                                                        x8 <- DM.constructorHook
                                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                   (DI.DynamicInfo [] []))
                                                                (Prelude.return Nil)
                                                        DM.constructorHook
                                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                             (DI.DynamicInfo []
                                                                [DI.genTerm x7, DI.genTerm x8]))
                                                          (Prelude.return (Tuple2 x7 x8))
                                              x13 <- do x9 <- Prelude.return x2
                                                        x10 <- Prelude.return x3
                                                        DM.constructorHook
                                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                             (DI.DynamicInfo []
                                                                [DI.genTerm x9, DI.genTerm x10]))
                                                          (Prelude.return (Cons x9 x10))
                                              DM.funcCallHook "foldr"
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x11, DI.genTerm x12,
                                                       DI.genTerm x13]))
                                                (strict_foldr x11 x12 x13)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x5 <- do x14 <- Prelude.return x4
                                                       DM.funcCallHook
                                                         "allValuesB.unfoldOrs.692._#selFP40#vals"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x14]))
                                                         (x'xstrict_allValuesB46unfoldOrs4669246_35selFP4035vals
                                                            x14)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x6 <- do x15 <- Prelude.return x4
                                                                DM.funcCallHook
                                                                  "allValuesB.unfoldOrs.692._#selFP41#ors"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x15]))
                                                                  (x'xstrict_allValuesB46unfoldOrs4669246_35selFP4135ors
                                                                     x15)
                                                       DM.eval
                                                         (do x17 <- Prelude.return x5
                                                             x18 <- do x16 <- Prelude.return x6
                                                                       DM.funcCallHook
                                                                         "allValuesB.unfoldOrs.692"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x16]))
                                                                         (x'xstrict_allValuesB46unfoldOrs46692
                                                                            x16)
                                                             DM.funcCallHook "++"
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x17,
                                                                      DI.genTerm x18]))
                                                               (op_PlusPlus x17 x18)))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x19])))
                           strict__case_2
                           x19)))
term_strict__case_2 x1
  = DI.Term "_case_2" (DI.SrcID "Prelude" 0) x1
strict__case_3 x1
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_3
                           x4)))
term_strict__case_3 x1
  = DI.Term "_case_3" (DI.SrcID "Prelude" 0) x1
strict__case_4 x1
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_4
                           x4)))
term_strict__case_4 x1
  = DI.Term "_case_4" (DI.SrcID "Prelude" 0) x1
strict__case_5 x1
  = DM.eval
      (DM.funcDeclHook "_case_5"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_5
                           x4)))
term_strict__case_5 x1
  = DI.Term "_case_5" (DI.SrcID "Prelude" 0) x1
strict__case_6 x1
  = DM.eval
      (DM.funcDeclHook "_case_6"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_6
                           x4)))
term_strict__case_6 x1
  = DI.Term "_case_6" (DI.SrcID "Prelude" 0) x1
strict__case_7 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_7"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x23 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x23]))
               (case x23 of
                    Value x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x4 <- Prelude.return x2
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x5 <- do x11 <- Prelude.return x4
                                                       DM.funcCallHook
                                                         "allValuesB.partition.692._#selFP34#vs"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x11]))
                                                         (x'xstrict_allValuesB46partition4669246_35selFP3435vs
                                                            x11)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x6 <- do x12 <- Prelude.return x4
                                                                DM.funcCallHook
                                                                  "allValuesB.partition.692._#selFP35#ors"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x12]))
                                                                  (x'xstrict_allValuesB46partition4669246_35selFP3535ors
                                                                     x12)
                                                       DM.eval
                                                         (do x15 <- do x13 <- Prelude.return x3
                                                                       x14 <- Prelude.return x5
                                                                       DM.constructorHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x13,
                                                                                DI.genTerm x14]))
                                                                         (Prelude.return
                                                                            (Cons x13 x14))
                                                             x16 <- Prelude.return x6
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x15,
                                                                      DI.genTerm x16]))
                                                               (Prelude.return
                                                                  (Tuple2 x15 x16))))))))))
                    Choice x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x8 <- Prelude.return x2
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x9 <- do x17 <- Prelude.return x8
                                                       DM.funcCallHook
                                                         "allValuesB.partition.692._#selFP37#vs"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x17]))
                                                         (x'xstrict_allValuesB46partition4669246_35selFP3735vs
                                                            x17)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x10 <- do x18 <- Prelude.return x8
                                                                 DM.funcCallHook
                                                                   "allValuesB.partition.692._#selFP38#ors"
                                                                   (DI.DebugInfo
                                                                      (DI.SrcID "Prelude" 0)
                                                                      (DI.DynamicInfo []
                                                                         [DI.genTerm x18]))
                                                                   (x'xstrict_allValuesB46partition4669246_35selFP3835ors
                                                                      x18)
                                                       DM.eval
                                                         (do x21 <- Prelude.return x9
                                                             x22 <- do x19 <- Prelude.return x7
                                                                       x20 <- Prelude.return x10
                                                                       DM.funcCallHook "++"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x19,
                                                                                DI.genTerm x20]))
                                                                         (op_PlusPlus x19 x20)
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x21,
                                                                      DI.genTerm x22]))
                                                               (Prelude.return
                                                                  (Tuple2 x21 x22))))))))))
                    Fail
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Suspend
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x23])))
                           (strict__case_7 x2)
                           x23)))
term_strict__case_7 x1
  = DI.Term "_case_7" (DI.SrcID "Prelude" 0) x1
strict__case_8 x1
  = DM.eval
      (DM.funcDeclHook "_case_8"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x9 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Value x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x2
                                  x5 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (Prelude.return (Cons x4 x5))))
                    Fail
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Suspend
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Choice x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- do x6 <- Prelude.return
                                                   (PC.partCall1 (term_strict_allValuesD [])
                                                      strict_allValuesD)
                                           DM.funcCallHook "concatMap"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6]))
                                             (strict_concatMap x6)
                                  x8 <- Prelude.return x3
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (strict_apply x7 x8)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           strict__case_8
                           x9)))
term_strict__case_8 x1
  = DI.Term "_case_8" (DI.SrcID "Prelude" 0) x1
strict__case_9 x1
  = DM.eval
      (DM.funcDeclHook "_case_9"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  DM.funcCallHook "return"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4]))
                                    (strict_return x4)))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- do x5 <- Prelude.return x3
                                           Prelude.return
                                             (PC.partCall1
                                                (x'xterm_strict_sequenceIO46_35lambda12
                                                   [DI.genTerm x5])
                                                (x'xstrict_sequenceIO46_35lambda12 x5))
                                  DM.funcCallHook ">>="
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (op_GtGtEq x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_9
                           x8)))
term_strict__case_9 x1
  = DI.Term "_case_9" (DI.SrcID "Prelude" 0) x1
strict__case_10 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_10"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x7 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  DM.funcCallHook "return"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3]))
                                    (strict_return x3)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- DM.funcCallHook "getLine"
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          strict_getLine
                                  x6 <- do x4 <- Prelude.return x1
                                           Prelude.return
                                             (PC.partCall1
                                                (x'xterm_strict_getLine46_35lambda1046_35lambda11
                                                   [DI.genTerm x4])
                                                (x'xstrict_getLine46_35lambda1046_35lambda11 x4))
                                  DM.funcCallHook ">>="
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (op_GtGtEq x5 x6)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_10 x1)
                           x7)))
term_strict__case_10 x1
  = DI.Term "_case_10" (DI.SrcID "Prelude" 0) x1
strict__case_11 x1
  = DM.eval
      (DM.funcDeclHook "_case_11"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "done"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_done))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- do x4 <- Prelude.return x2
                                           DM.funcCallHook "putChar"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4]))
                                             (strict_putChar x4)
                                  x7 <- do x5 <- Prelude.return x3
                                           DM.funcCallHook "putStr"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (strict_putStr x5)
                                  DM.funcCallHook ">>"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (op_GtGt x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_11
                           x8)))
term_strict__case_11 x1
  = DI.Term "_case_11" (DI.SrcID "Prelude" 0) x1
strict__case_12 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_12"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x10 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Left x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict_apply x6 x7)))
                    Right x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x2
                                  x9 <- Prelude.return x5
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (strict_apply x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_12 x1 x2)
                           x10)))
term_strict__case_12 x1
  = DI.Term "_case_12" (DI.SrcID "Prelude" 0) x1
strict__case_13 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_13"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x7 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Nothing
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    Just x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x2
                                  x6 <- Prelude.return x4
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (strict_apply x5 x6)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_13 x1 x2)
                           x7)))
term_strict__case_13 x1
  = DI.Term "_case_13" (DI.SrcID "Prelude" 0) x1
strict__case_14 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_14"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "success"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_success))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_14 x1 x2)
                           x4)))
term_strict__case_14 x1
  = DI.Term "_case_14" (DI.SrcID "Prelude" 0) x1
strict__case_15 x1
  = DM.eval
      (DM.funcDeclHook "_case_15"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x6 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Zero)))
                    Pos x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x2
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4]))
                                    (Prelude.return (Neg x4))))
                    Neg x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x3
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5]))
                                    (Prelude.return (Pos x5))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           strict__case_15
                           x6)))
term_strict__case_15 x1
  = DI.Term "_case_15" (DI.SrcID "Prelude" 0) x1
strict__case_16 x1
  = DM.eval
      (DM.funcDeclHook "_case_16"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_16
                           x4)))
term_strict__case_16 x1
  = DI.Term "_case_16" (DI.SrcID "Prelude" 0) x1
strict__case_17 x1
  = DM.eval
      (DM.funcDeclHook "_case_17"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_17
                           x4)))
term_strict__case_17 x1
  = DI.Term "_case_17" (DI.SrcID "Prelude" 0) x1
strict__case_18 x1
  = DM.eval
      (DM.funcDeclHook "_case_18"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_18
                           x4)))
term_strict__case_18 x1
  = DI.Term "_case_18" (DI.SrcID "Prelude" 0) x1
strict__case_19 x1
  = DM.eval
      (DM.funcDeclHook "_case_19"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_19
                           x4)))
term_strict__case_19 x1
  = DI.Term "_case_19" (DI.SrcID "Prelude" 0) x1
strict__case_20 x1
  = DM.eval
      (DM.funcDeclHook "_case_20"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_20
                           x4)))
term_strict__case_20 x1
  = DI.Term "_case_20" (DI.SrcID "Prelude" 0) x1
strict__case_21 x1
  = DM.eval
      (DM.funcDeclHook "_case_21"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_21
                           x4)))
term_strict__case_21 x1
  = DI.Term "_case_21" (DI.SrcID "Prelude" 0) x1
strict__case_24 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_24"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x16 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- DM.constructorHook
                                           (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                              (DI.DynamicInfo [] []))
                                           (Prelude.return Zero)
                                  x11 <- DM.constructorHook
                                           (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                              (DI.DynamicInfo [] []))
                                           (Prelude.return Zero)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (Prelude.return (Tuple2 x10 x11))))
                    Pos x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x3
                                  x13 <- Prelude.return x2
                                  DM.funcCallHook "_case_23"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (strict__case_23 x12 x13)))
                    Neg x9
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- Prelude.return x9
                                  x15 <- Prelude.return x2
                                  DM.funcCallHook "_case_22"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (strict__case_22 x14 x15)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_24 x2)
                           x16)))
term_strict__case_24 x1
  = DI.Term "_case_24" (DI.SrcID "Prelude" 0) x1
strict__case_22 x9 x2
  = DM.eval
      (DM.funcDeclHook "_case_22"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x2]))
         (do x60 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x60]))
               (case x60 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x44 <- do x42 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return (Char 'd'))
                                            x43 <- do x40 <- DM.litHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo [] []))
                                                               (Prelude.return (Char 'i'))
                                                      x41 <- do x38 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return (Char 'v'))
                                                                x39 <- do x36 <- DM.litHook
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Prelude"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         []))
                                                                                   (Prelude.return
                                                                                      (Char 'i'))
                                                                          x37 <- do x34 <- DM.litHook
                                                                                             (DI.DebugInfo
                                                                                                (DI.SrcID
                                                                                                   "Prelude"
                                                                                                   0)
                                                                                                (DI.DynamicInfo
                                                                                                   []
                                                                                                   []))
                                                                                             (Prelude.return
                                                                                                (Char
                                                                                                   's'))
                                                                                    x35 <- do x32 <- DM.litHook
                                                                                                       (DI.DebugInfo
                                                                                                          (DI.SrcID
                                                                                                             "Prelude"
                                                                                                             0)
                                                                                                          (DI.DynamicInfo
                                                                                                             []
                                                                                                             []))
                                                                                                       (Prelude.return
                                                                                                          (Char
                                                                                                             'i'))
                                                                                              x33 <- do x30 <- DM.litHook
                                                                                                                 (DI.DebugInfo
                                                                                                                    (DI.SrcID
                                                                                                                       "Prelude"
                                                                                                                       0)
                                                                                                                    (DI.DynamicInfo
                                                                                                                       []
                                                                                                                       []))
                                                                                                                 (Prelude.return
                                                                                                                    (Char
                                                                                                                       'o'))
                                                                                                        x31 <- do x28 <- DM.litHook
                                                                                                                           (DI.DebugInfo
                                                                                                                              (DI.SrcID
                                                                                                                                 "Prelude"
                                                                                                                                 0)
                                                                                                                              (DI.DynamicInfo
                                                                                                                                 []
                                                                                                                                 []))
                                                                                                                           (Prelude.return
                                                                                                                              (Char
                                                                                                                                 'n'))
                                                                                                                  x29 <- do x26 <- DM.litHook
                                                                                                                                     (DI.DebugInfo
                                                                                                                                        (DI.SrcID
                                                                                                                                           "Prelude"
                                                                                                                                           0)
                                                                                                                                        (DI.DynamicInfo
                                                                                                                                           []
                                                                                                                                           []))
                                                                                                                                     (Prelude.return
                                                                                                                                        (Char
                                                                                                                                           ' '))
                                                                                                                            x27 <- do x24 <- DM.litHook
                                                                                                                                               (DI.DebugInfo
                                                                                                                                                  (DI.SrcID
                                                                                                                                                     "Prelude"
                                                                                                                                                     0)
                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                     []
                                                                                                                                                     []))
                                                                                                                                               (Prelude.return
                                                                                                                                                  (Char
                                                                                                                                                     'b'))
                                                                                                                                      x25 <- do x22 <- DM.litHook
                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                            (DI.SrcID
                                                                                                                                                               "Prelude"
                                                                                                                                                               0)
                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                               []
                                                                                                                                                               []))
                                                                                                                                                         (Prelude.return
                                                                                                                                                            (Char
                                                                                                                                                               'y'))
                                                                                                                                                x23 <- do x20 <- DM.litHook
                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                         "Prelude"
                                                                                                                                                                         0)
                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                         []
                                                                                                                                                                         []))
                                                                                                                                                                   (Prelude.return
                                                                                                                                                                      (Char
                                                                                                                                                                         ' '))
                                                                                                                                                          x21 <- do x18 <- DM.litHook
                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                   "Prelude"
                                                                                                                                                                                   0)
                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                   []
                                                                                                                                                                                   []))
                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                (Char
                                                                                                                                                                                   '0'))
                                                                                                                                                                    x19 <- DM.constructorHook
                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                   "Prelude"
                                                                                                                                                                                   0)
                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                   []
                                                                                                                                                                                   []))
                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                Nil)
                                                                                                                                                                    DM.constructorHook
                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                            "Prelude"
                                                                                                                                                                            0)
                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                            []
                                                                                                                                                                            [DI.genTerm
                                                                                                                                                                               x18,
                                                                                                                                                                             DI.genTerm
                                                                                                                                                                               x19]))
                                                                                                                                                                      (Prelude.return
                                                                                                                                                                         (Cons
                                                                                                                                                                            x18
                                                                                                                                                                            x19))
                                                                                                                                                          DM.constructorHook
                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                               (DI.SrcID
                                                                                                                                                                  "Prelude"
                                                                                                                                                                  0)
                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                  []
                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                     x20,
                                                                                                                                                                   DI.genTerm
                                                                                                                                                                     x21]))
                                                                                                                                                            (Prelude.return
                                                                                                                                                               (Cons
                                                                                                                                                                  x20
                                                                                                                                                                  x21))
                                                                                                                                                DM.constructorHook
                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                     (DI.SrcID
                                                                                                                                                        "Prelude"
                                                                                                                                                        0)
                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                        []
                                                                                                                                                        [DI.genTerm
                                                                                                                                                           x22,
                                                                                                                                                         DI.genTerm
                                                                                                                                                           x23]))
                                                                                                                                                  (Prelude.return
                                                                                                                                                     (Cons
                                                                                                                                                        x22
                                                                                                                                                        x23))
                                                                                                                                      DM.constructorHook
                                                                                                                                        (DI.DebugInfo
                                                                                                                                           (DI.SrcID
                                                                                                                                              "Prelude"
                                                                                                                                              0)
                                                                                                                                           (DI.DynamicInfo
                                                                                                                                              []
                                                                                                                                              [DI.genTerm
                                                                                                                                                 x24,
                                                                                                                                               DI.genTerm
                                                                                                                                                 x25]))
                                                                                                                                        (Prelude.return
                                                                                                                                           (Cons
                                                                                                                                              x24
                                                                                                                                              x25))
                                                                                                                            DM.constructorHook
                                                                                                                              (DI.DebugInfo
                                                                                                                                 (DI.SrcID
                                                                                                                                    "Prelude"
                                                                                                                                    0)
                                                                                                                                 (DI.DynamicInfo
                                                                                                                                    []
                                                                                                                                    [DI.genTerm
                                                                                                                                       x26,
                                                                                                                                     DI.genTerm
                                                                                                                                       x27]))
                                                                                                                              (Prelude.return
                                                                                                                                 (Cons
                                                                                                                                    x26
                                                                                                                                    x27))
                                                                                                                  DM.constructorHook
                                                                                                                    (DI.DebugInfo
                                                                                                                       (DI.SrcID
                                                                                                                          "Prelude"
                                                                                                                          0)
                                                                                                                       (DI.DynamicInfo
                                                                                                                          []
                                                                                                                          [DI.genTerm
                                                                                                                             x28,
                                                                                                                           DI.genTerm
                                                                                                                             x29]))
                                                                                                                    (Prelude.return
                                                                                                                       (Cons
                                                                                                                          x28
                                                                                                                          x29))
                                                                                                        DM.constructorHook
                                                                                                          (DI.DebugInfo
                                                                                                             (DI.SrcID
                                                                                                                "Prelude"
                                                                                                                0)
                                                                                                             (DI.DynamicInfo
                                                                                                                []
                                                                                                                [DI.genTerm
                                                                                                                   x30,
                                                                                                                 DI.genTerm
                                                                                                                   x31]))
                                                                                                          (Prelude.return
                                                                                                             (Cons
                                                                                                                x30
                                                                                                                x31))
                                                                                              DM.constructorHook
                                                                                                (DI.DebugInfo
                                                                                                   (DI.SrcID
                                                                                                      "Prelude"
                                                                                                      0)
                                                                                                   (DI.DynamicInfo
                                                                                                      []
                                                                                                      [DI.genTerm
                                                                                                         x32,
                                                                                                       DI.genTerm
                                                                                                         x33]))
                                                                                                (Prelude.return
                                                                                                   (Cons
                                                                                                      x32
                                                                                                      x33))
                                                                                    DM.constructorHook
                                                                                      (DI.DebugInfo
                                                                                         (DI.SrcID
                                                                                            "Prelude"
                                                                                            0)
                                                                                         (DI.DynamicInfo
                                                                                            []
                                                                                            [DI.genTerm
                                                                                               x34,
                                                                                             DI.genTerm
                                                                                               x35]))
                                                                                      (Prelude.return
                                                                                         (Cons x34
                                                                                            x35))
                                                                          DM.constructorHook
                                                                            (DI.DebugInfo
                                                                               (DI.SrcID "Prelude"
                                                                                  0)
                                                                               (DI.DynamicInfo []
                                                                                  [DI.genTerm x36,
                                                                                   DI.genTerm x37]))
                                                                            (Prelude.return
                                                                               (Cons x36 x37))
                                                                DM.constructorHook
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x38,
                                                                         DI.genTerm x39]))
                                                                  (Prelude.return (Cons x38 x39))
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x40, DI.genTerm x41]))
                                                        (Prelude.return (Cons x40 x41))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x42, DI.genTerm x43]))
                                              (Prelude.return (Cons x42 x43))
                                  DM.funcCallHook "error"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x44]))
                                    (strict_error x44)))
                    Pos x10
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x11 <- do x45 <- Prelude.return x9
                                               x46 <- Prelude.return x10
                                               DM.funcCallHook "divmodNat"
                                                 (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                    (DI.DynamicInfo []
                                                       [DI.genTerm x45, DI.genTerm x46]))
                                                 (strict_divmodNat x45 x46)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x12 <- do x47 <- Prelude.return x11
                                                        DM.funcCallHook "divmod._#selFP28#d"
                                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                             (DI.DynamicInfo [] [DI.genTerm x47]))
                                                          (x'xstrict_divmod46_35selFP2835d x47)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x13 <- do x48 <- Prelude.return x11
                                                                 DM.funcCallHook
                                                                   "divmod._#selFP29#m"
                                                                   (DI.DebugInfo
                                                                      (DI.SrcID "Prelude" 0)
                                                                      (DI.DynamicInfo []
                                                                         [DI.genTerm x48]))
                                                                   (x'xstrict_divmod46_35selFP2935m
                                                                      x48)
                                                       DM.eval
                                                         (do x51 <- do x49 <- Prelude.return x12
                                                                       DM.funcCallHook "negate"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x49]))
                                                                         (strict_negate x49)
                                                             x52 <- do x50 <- Prelude.return x13
                                                                       DM.funcCallHook "negate"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x50]))
                                                                         (strict_negate x50)
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x51,
                                                                      DI.genTerm x52]))
                                                               (Prelude.return
                                                                  (Tuple2 x51 x52))))))))))
                    Neg x14
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x15 <- do x53 <- Prelude.return x9
                                               x54 <- Prelude.return x14
                                               DM.funcCallHook "divmodNat"
                                                 (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                    (DI.DynamicInfo []
                                                       [DI.genTerm x53, DI.genTerm x54]))
                                                 (strict_divmodNat x53 x54)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x16 <- do x55 <- Prelude.return x15
                                                        DM.funcCallHook "divmod._#selFP31#d"
                                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                             (DI.DynamicInfo [] [DI.genTerm x55]))
                                                          (x'xstrict_divmod46_35selFP3135d x55)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x17 <- do x56 <- Prelude.return x15
                                                                 DM.funcCallHook
                                                                   "divmod._#selFP32#m"
                                                                   (DI.DebugInfo
                                                                      (DI.SrcID "Prelude" 0)
                                                                      (DI.DynamicInfo []
                                                                         [DI.genTerm x56]))
                                                                   (x'xstrict_divmod46_35selFP3235m
                                                                      x56)
                                                       DM.eval
                                                         (do x58 <- Prelude.return x16
                                                             x59 <- do x57 <- Prelude.return x17
                                                                       DM.funcCallHook "negate"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x57]))
                                                                         (strict_negate x57)
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x58,
                                                                      DI.genTerm x59]))
                                                               (Prelude.return
                                                                  (Tuple2 x58 x59))))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x60])))
                           (strict__case_22 x9)
                           x60)))
term_strict__case_22 x1
  = DI.Term "_case_22" (DI.SrcID "Prelude" 0) x1
strict__case_23 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_23"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x2]))
         (do x45 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x45]))
               (case x45 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x35 <- do x33 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return (Char 'd'))
                                            x34 <- do x31 <- DM.litHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo [] []))
                                                               (Prelude.return (Char 'i'))
                                                      x32 <- do x29 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return (Char 'v'))
                                                                x30 <- do x27 <- DM.litHook
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Prelude"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         []))
                                                                                   (Prelude.return
                                                                                      (Char 'i'))
                                                                          x28 <- do x25 <- DM.litHook
                                                                                             (DI.DebugInfo
                                                                                                (DI.SrcID
                                                                                                   "Prelude"
                                                                                                   0)
                                                                                                (DI.DynamicInfo
                                                                                                   []
                                                                                                   []))
                                                                                             (Prelude.return
                                                                                                (Char
                                                                                                   's'))
                                                                                    x26 <- do x23 <- DM.litHook
                                                                                                       (DI.DebugInfo
                                                                                                          (DI.SrcID
                                                                                                             "Prelude"
                                                                                                             0)
                                                                                                          (DI.DynamicInfo
                                                                                                             []
                                                                                                             []))
                                                                                                       (Prelude.return
                                                                                                          (Char
                                                                                                             'i'))
                                                                                              x24 <- do x21 <- DM.litHook
                                                                                                                 (DI.DebugInfo
                                                                                                                    (DI.SrcID
                                                                                                                       "Prelude"
                                                                                                                       0)
                                                                                                                    (DI.DynamicInfo
                                                                                                                       []
                                                                                                                       []))
                                                                                                                 (Prelude.return
                                                                                                                    (Char
                                                                                                                       'o'))
                                                                                                        x22 <- do x19 <- DM.litHook
                                                                                                                           (DI.DebugInfo
                                                                                                                              (DI.SrcID
                                                                                                                                 "Prelude"
                                                                                                                                 0)
                                                                                                                              (DI.DynamicInfo
                                                                                                                                 []
                                                                                                                                 []))
                                                                                                                           (Prelude.return
                                                                                                                              (Char
                                                                                                                                 'n'))
                                                                                                                  x20 <- do x17 <- DM.litHook
                                                                                                                                     (DI.DebugInfo
                                                                                                                                        (DI.SrcID
                                                                                                                                           "Prelude"
                                                                                                                                           0)
                                                                                                                                        (DI.DynamicInfo
                                                                                                                                           []
                                                                                                                                           []))
                                                                                                                                     (Prelude.return
                                                                                                                                        (Char
                                                                                                                                           ' '))
                                                                                                                            x18 <- do x15 <- DM.litHook
                                                                                                                                               (DI.DebugInfo
                                                                                                                                                  (DI.SrcID
                                                                                                                                                     "Prelude"
                                                                                                                                                     0)
                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                     []
                                                                                                                                                     []))
                                                                                                                                               (Prelude.return
                                                                                                                                                  (Char
                                                                                                                                                     'b'))
                                                                                                                                      x16 <- do x13 <- DM.litHook
                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                            (DI.SrcID
                                                                                                                                                               "Prelude"
                                                                                                                                                               0)
                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                               []
                                                                                                                                                               []))
                                                                                                                                                         (Prelude.return
                                                                                                                                                            (Char
                                                                                                                                                               'y'))
                                                                                                                                                x14 <- do x11 <- DM.litHook
                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                         "Prelude"
                                                                                                                                                                         0)
                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                         []
                                                                                                                                                                         []))
                                                                                                                                                                   (Prelude.return
                                                                                                                                                                      (Char
                                                                                                                                                                         ' '))
                                                                                                                                                          x12 <- do x9 <- DM.litHook
                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                  "Prelude"
                                                                                                                                                                                  0)
                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                  []
                                                                                                                                                                                  []))
                                                                                                                                                                            (Prelude.return
                                                                                                                                                                               (Char
                                                                                                                                                                                  '0'))
                                                                                                                                                                    x10 <- DM.constructorHook
                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                   "Prelude"
                                                                                                                                                                                   0)
                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                   []
                                                                                                                                                                                   []))
                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                Nil)
                                                                                                                                                                    DM.constructorHook
                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                            "Prelude"
                                                                                                                                                                            0)
                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                            []
                                                                                                                                                                            [DI.genTerm
                                                                                                                                                                               x9,
                                                                                                                                                                             DI.genTerm
                                                                                                                                                                               x10]))
                                                                                                                                                                      (Prelude.return
                                                                                                                                                                         (Cons
                                                                                                                                                                            x9
                                                                                                                                                                            x10))
                                                                                                                                                          DM.constructorHook
                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                               (DI.SrcID
                                                                                                                                                                  "Prelude"
                                                                                                                                                                  0)
                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                  []
                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                     x11,
                                                                                                                                                                   DI.genTerm
                                                                                                                                                                     x12]))
                                                                                                                                                            (Prelude.return
                                                                                                                                                               (Cons
                                                                                                                                                                  x11
                                                                                                                                                                  x12))
                                                                                                                                                DM.constructorHook
                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                     (DI.SrcID
                                                                                                                                                        "Prelude"
                                                                                                                                                        0)
                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                        []
                                                                                                                                                        [DI.genTerm
                                                                                                                                                           x13,
                                                                                                                                                         DI.genTerm
                                                                                                                                                           x14]))
                                                                                                                                                  (Prelude.return
                                                                                                                                                     (Cons
                                                                                                                                                        x13
                                                                                                                                                        x14))
                                                                                                                                      DM.constructorHook
                                                                                                                                        (DI.DebugInfo
                                                                                                                                           (DI.SrcID
                                                                                                                                              "Prelude"
                                                                                                                                              0)
                                                                                                                                           (DI.DynamicInfo
                                                                                                                                              []
                                                                                                                                              [DI.genTerm
                                                                                                                                                 x15,
                                                                                                                                               DI.genTerm
                                                                                                                                                 x16]))
                                                                                                                                        (Prelude.return
                                                                                                                                           (Cons
                                                                                                                                              x15
                                                                                                                                              x16))
                                                                                                                            DM.constructorHook
                                                                                                                              (DI.DebugInfo
                                                                                                                                 (DI.SrcID
                                                                                                                                    "Prelude"
                                                                                                                                    0)
                                                                                                                                 (DI.DynamicInfo
                                                                                                                                    []
                                                                                                                                    [DI.genTerm
                                                                                                                                       x17,
                                                                                                                                     DI.genTerm
                                                                                                                                       x18]))
                                                                                                                              (Prelude.return
                                                                                                                                 (Cons
                                                                                                                                    x17
                                                                                                                                    x18))
                                                                                                                  DM.constructorHook
                                                                                                                    (DI.DebugInfo
                                                                                                                       (DI.SrcID
                                                                                                                          "Prelude"
                                                                                                                          0)
                                                                                                                       (DI.DynamicInfo
                                                                                                                          []
                                                                                                                          [DI.genTerm
                                                                                                                             x19,
                                                                                                                           DI.genTerm
                                                                                                                             x20]))
                                                                                                                    (Prelude.return
                                                                                                                       (Cons
                                                                                                                          x19
                                                                                                                          x20))
                                                                                                        DM.constructorHook
                                                                                                          (DI.DebugInfo
                                                                                                             (DI.SrcID
                                                                                                                "Prelude"
                                                                                                                0)
                                                                                                             (DI.DynamicInfo
                                                                                                                []
                                                                                                                [DI.genTerm
                                                                                                                   x21,
                                                                                                                 DI.genTerm
                                                                                                                   x22]))
                                                                                                          (Prelude.return
                                                                                                             (Cons
                                                                                                                x21
                                                                                                                x22))
                                                                                              DM.constructorHook
                                                                                                (DI.DebugInfo
                                                                                                   (DI.SrcID
                                                                                                      "Prelude"
                                                                                                      0)
                                                                                                   (DI.DynamicInfo
                                                                                                      []
                                                                                                      [DI.genTerm
                                                                                                         x23,
                                                                                                       DI.genTerm
                                                                                                         x24]))
                                                                                                (Prelude.return
                                                                                                   (Cons
                                                                                                      x23
                                                                                                      x24))
                                                                                    DM.constructorHook
                                                                                      (DI.DebugInfo
                                                                                         (DI.SrcID
                                                                                            "Prelude"
                                                                                            0)
                                                                                         (DI.DynamicInfo
                                                                                            []
                                                                                            [DI.genTerm
                                                                                               x25,
                                                                                             DI.genTerm
                                                                                               x26]))
                                                                                      (Prelude.return
                                                                                         (Cons x25
                                                                                            x26))
                                                                          DM.constructorHook
                                                                            (DI.DebugInfo
                                                                               (DI.SrcID "Prelude"
                                                                                  0)
                                                                               (DI.DynamicInfo []
                                                                                  [DI.genTerm x27,
                                                                                   DI.genTerm x28]))
                                                                            (Prelude.return
                                                                               (Cons x27 x28))
                                                                DM.constructorHook
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x29,
                                                                         DI.genTerm x30]))
                                                                  (Prelude.return (Cons x29 x30))
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x31, DI.genTerm x32]))
                                                        (Prelude.return (Cons x31 x32))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x33, DI.genTerm x34]))
                                              (Prelude.return (Cons x33 x34))
                                  DM.funcCallHook "error"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x35]))
                                    (strict_error x35)))
                    Pos x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x36 <- Prelude.return x3
                                  x37 <- Prelude.return x4
                                  DM.funcCallHook "divmodNat"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x36, DI.genTerm x37]))
                                    (strict_divmodNat x36 x37)))
                    Neg x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x6 <- do x38 <- Prelude.return x3
                                              x39 <- Prelude.return x5
                                              DM.funcCallHook "divmodNat"
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x38, DI.genTerm x39]))
                                                (strict_divmodNat x38 x39)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x7 <- do x40 <- Prelude.return x6
                                                       DM.funcCallHook "divmod._#selFP25#d"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x40]))
                                                         (x'xstrict_divmod46_35selFP2535d x40)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x8 <- do x41 <- Prelude.return x6
                                                                DM.funcCallHook "divmod._#selFP26#m"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x41]))
                                                                  (x'xstrict_divmod46_35selFP2635m
                                                                     x41)
                                                       DM.eval
                                                         (do x43 <- do x42 <- Prelude.return x7
                                                                       DM.funcCallHook "negate"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x42]))
                                                                         (strict_negate x42)
                                                             x44 <- Prelude.return x8
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x43,
                                                                      DI.genTerm x44]))
                                                               (Prelude.return
                                                                  (Tuple2 x43 x44))))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x45])))
                           (strict__case_23 x3)
                           x45)))
term_strict__case_23 x1
  = DI.Term "_case_23" (DI.SrcID "Prelude" 0) x1
strict__case_27 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_27"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x11 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Pos x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  x8 <- Prelude.return x2
                                  DM.funcCallHook "_case_26"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (strict__case_26 x7 x8)))
                    Neg x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x6
                                  x10 <- Prelude.return x2
                                  DM.funcCallHook "_case_25"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_25 x9 x10)))
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Zero)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_27 x2)
                           x11)))
term_strict__case_27 x1
  = DI.Term "_case_27" (DI.SrcID "Prelude" 0) x1
strict__case_25 x6 x2
  = DM.eval
      (DM.funcDeclHook "_case_25"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x2]))
         (do x15 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Neg x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x9 <- Prelude.return x6
                                            x10 <- Prelude.return x7
                                            DM.funcCallHook "*^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (op_AsteriskAccent x9 x10)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11]))
                                    (Prelude.return (Pos x11))))
                    Pos x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- do x12 <- Prelude.return x6
                                            x13 <- Prelude.return x8
                                            DM.funcCallHook "*^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x12, DI.genTerm x13]))
                                              (op_AsteriskAccent x12 x13)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14]))
                                    (Prelude.return (Neg x14))))
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Zero)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_25 x6)
                           x15)))
term_strict__case_25 x1
  = DI.Term "_case_25" (DI.SrcID "Prelude" 0) x1
strict__case_26 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_26"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x2]))
         (do x12 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Pos x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x6 <- Prelude.return x3
                                           x7 <- Prelude.return x4
                                           DM.funcCallHook "*^"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (op_AsteriskAccent x6 x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (Prelude.return (Pos x8))))
                    Neg x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x9 <- Prelude.return x3
                                            x10 <- Prelude.return x5
                                            DM.funcCallHook "*^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (op_AsteriskAccent x9 x10)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11]))
                                    (Prelude.return (Neg x11))))
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Zero)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_26 x3)
                           x12)))
term_strict__case_26 x1
  = DI.Term "_case_26" (DI.SrcID "Prelude" 0) x1
strict__case_28 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_28"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x11 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Neg x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- do x5 <- Prelude.return x3
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (Prelude.return (Pos x5))
                                  DM.funcCallHook "+"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (op_Plus x6 x7)))
                    Pos x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- do x8 <- Prelude.return x4
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8]))
                                              (Prelude.return (Neg x8))
                                  DM.funcCallHook "+"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (op_Plus x9 x10)))
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_28 x1)
                           x11)))
term_strict__case_28 x1
  = DI.Term "_case_28" (DI.SrcID "Prelude" 0) x1
strict__case_31 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_31"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x13 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Pos x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x3
                                  x9 <- Prelude.return x2
                                  DM.funcCallHook "_case_30"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
                                    (strict__case_30 x7 x8 x9)))
                    Neg x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x1
                                  x11 <- Prelude.return x6
                                  x12 <- Prelude.return x2
                                  DM.funcCallHook "_case_29"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x10, DI.genTerm x11, DI.genTerm x12]))
                                    (strict__case_29 x10 x11 x12)))
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_31 x2)
                           x13)))
term_strict__case_31 x1
  = DI.Term "_case_31" (DI.SrcID "Prelude" 0) x1
strict__case_29 x1 x6 x2
  = DM.eval
      (DM.funcDeclHook "_case_29"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x6, DI.genTerm x2]))
         (do x14 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Neg x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x9 <- Prelude.return x6
                                            x10 <- Prelude.return x7
                                            DM.funcCallHook "+^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (op_PlusAccent x9 x10)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11]))
                                    (Prelude.return (Neg x11))))
                    Pos x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x8
                                  x13 <- Prelude.return x6
                                  DM.funcCallHook "-^"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (op_MinusAccent x12 x13)))
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_29 x1 x6)
                           x14)))
term_strict__case_29 x1
  = DI.Term "_case_29" (DI.SrcID "Prelude" 0) x1
strict__case_30 x1 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_30"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x2]))
         (do x11 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Pos x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x6 <- Prelude.return x3
                                           x7 <- Prelude.return x4
                                           DM.funcCallHook "+^"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (op_PlusAccent x6 x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (Prelude.return (Pos x8))))
                    Neg x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x3
                                  x10 <- Prelude.return x5
                                  DM.funcCallHook "-^"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (op_MinusAccent x9 x10)))
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_30 x1 x3)
                           x11)))
term_strict__case_30 x1
  = DI.Term "_case_30" (DI.SrcID "Prelude" 0) x1
strict__case_32 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_32"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x7 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    O x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x2
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5]))
                                    (Prelude.return (O x5))))
                    I x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Prelude.return (I x6))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_32 x2)
                           x7)))
term_strict__case_32 x1
  = DI.Term "_case_32" (DI.SrcID "Prelude" 0) x1
strict__case_40 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_40"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x10 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- do x4 <- Prelude.return x1
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4]))
                                             (Prelude.return (Pos x4))
                                  x6 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Zero)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (Prelude.return (Tuple2 x5 x6))))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x2
                                  x9 <- DM.funcCallHook "otherwise"
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          strict_otherwise
                                  DM.funcCallHook "_case_39"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
                                    (strict__case_39 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_40 x1 x2)
                           x10)))
term_strict__case_40 x1
  = DI.Term "_case_40" (DI.SrcID "Prelude" 0) x1
strict__case_39 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_39"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x9 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x2
                                  x8 <- do x4 <- Prelude.return x1
                                           x5 <- Prelude.return x2
                                           DM.funcCallHook "cmpNat"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (strict_cmpNat x4 x5)
                                  DM.funcCallHook "_case_38"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8]))
                                    (strict__case_38 x6 x7 x8)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           (strict__case_39 x1 x2)
                           x9)))
term_strict__case_39 x1
  = DI.Term "_case_39" (DI.SrcID "Prelude" 0) x1
strict__case_38 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_38"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x16 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    EQ
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- do x4 <- DM.constructorHook
                                                   (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return IHi)
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4]))
                                             (Prelude.return (Pos x4))
                                  x6 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Zero)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (Prelude.return (Tuple2 x5 x6))))
                    LT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Zero)
                                  x9 <- do x7 <- Prelude.return x1
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7]))
                                             (Prelude.return (Pos x7))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Tuple2 x8 x9))))
                    GT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- Prelude.return x1
                                  x14 <- Prelude.return x2
                                  x15 <- do x11 <- do x10 <- Prelude.return x1
                                                      DM.funcCallHook "div2"
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x10]))
                                                        (strict_div2 x10)
                                            x12 <- Prelude.return x2
                                            DM.funcCallHook "divmodNat"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12]))
                                              (strict_divmodNat x11 x12)
                                  DM.funcCallHook "_case_37"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x13, DI.genTerm x14, DI.genTerm x15]))
                                    (strict__case_37 x13 x14 x15)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_38 x1 x2)
                           x16)))
term_strict__case_38 x1
  = DI.Term "_case_38" (DI.SrcID "Prelude" 0) x1
strict__case_37 x1 x2 x5
  = DM.eval
      (DM.funcDeclHook "_case_37"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x5]))
         (do x10 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Tuple2 x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x2
                                  x8 <- Prelude.return x4
                                  x9 <- Prelude.return x3
                                  DM.funcCallHook "_case_36"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (strict__case_36 x6 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_37 x1 x2)
                           x10)))
term_strict__case_37 x1
  = DI.Term "_case_37" (DI.SrcID "Prelude" 0) x1
strict__case_36 x1 x2 x4 x3
  = DM.eval
      (DM.funcDeclHook "_case_36"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x4, DI.genTerm x3]))
         (do x22 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x22]))
               (case x22 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x16 <- do x13 <- DM.constructorHook
                                                     (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return IHi)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x13]))
                                              (Prelude.return (Pos x13))
                                  x17 <- do x14 <- Prelude.return x1
                                            x15 <- Prelude.return x2
                                            DM.funcCallHook "-^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x14, DI.genTerm x15]))
                                              (op_MinusAccent x14 x15)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x16, DI.genTerm x17]))
                                    (Prelude.return (Tuple2 x16 x17))))
                    Pos x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x18 <- Prelude.return x1
                                  x19 <- Prelude.return x2
                                  x20 <- Prelude.return x5
                                  x21 <- Prelude.return x4
                                  DM.funcCallHook "_case_35"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x18, DI.genTerm x19, DI.genTerm x20,
                                           DI.genTerm x21]))
                                    (strict__case_35 x18 x19 x20 x21)))
                    Neg x12
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x22])))
                           (strict__case_36 x1 x2 x4)
                           x22)))
term_strict__case_36 x1
  = DI.Term "_case_36" (DI.SrcID "Prelude" 0) x1
strict__case_35 x1 x2 x5 x4
  = DM.eval
      (DM.funcDeclHook "_case_35"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x5, DI.genTerm x4]))
         (do x26 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x26]))
               (case x26 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x15 <- do x13 <- do x12 <- Prelude.return x5
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x12]))
                                                        (Prelude.return (O x12))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x13]))
                                              (Prelude.return (Pos x13))
                                  x16 <- do x14 <- Prelude.return x1
                                            DM.funcCallHook "mod2"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x14]))
                                              (strict_mod2 x14)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x15, DI.genTerm x16]))
                                    (Prelude.return (Tuple2 x15 x16))))
                    Pos x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x21 <- Prelude.return x1
                                  x22 <- Prelude.return x2
                                  x23 <- Prelude.return x5
                                  x24 <- Prelude.return x6
                                  x25 <- do x19 <- do x17 <- Prelude.return x1
                                                      x18 <- Prelude.return x6
                                                      DM.funcCallHook "divmodNat.shift.523"
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x17, DI.genTerm x18]))
                                                        (x'xstrict_divmodNat46shift46523 x17 x18)
                                            x20 <- Prelude.return x2
                                            DM.funcCallHook "divmodNat"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x19, DI.genTerm x20]))
                                              (strict_divmodNat x19 x20)
                                  DM.funcCallHook "_case_34"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x21, DI.genTerm x22, DI.genTerm x23,
                                           DI.genTerm x24, DI.genTerm x25]))
                                    (strict__case_34 x21 x22 x23 x24 x25)))
                    Neg x11
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x26])))
                           (strict__case_35 x1 x2 x5)
                           x26)))
term_strict__case_35 x1
  = DI.Term "_case_35" (DI.SrcID "Prelude" 0) x1
strict__case_34 x1 x2 x5 x6 x9
  = DM.eval
      (DM.funcDeclHook "_case_34"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x5, DI.genTerm x6,
                DI.genTerm x9]))
         (do x13 <- Prelude.return x9
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Tuple2 x7 x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x5
                                  x11 <- Prelude.return x8
                                  x12 <- Prelude.return x7
                                  DM.funcCallHook "_case_33"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x10, DI.genTerm x11, DI.genTerm x12]))
                                    (strict__case_33 x10 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_34 x1 x2 x5 x6)
                           x13)))
term_strict__case_34 x1
  = DI.Term "_case_34" (DI.SrcID "Prelude" 0) x1
strict__case_33 x5 x8 x7
  = DM.eval
      (DM.funcDeclHook "_case_33"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x8, DI.genTerm x7]))
         (do x21 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x21]))
               (case x21 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- do x12 <- do x11 <- Prelude.return x5
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x11]))
                                                        (Prelude.return (O x11))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x12]))
                                              (Prelude.return (Pos x12))
                                  x14 <- Prelude.return x8
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x13, DI.genTerm x14]))
                                    (Prelude.return (Tuple2 x13 x14))))
                    Pos x9
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x19 <- do x18 <- do x16 <- do x15 <- Prelude.return x5
                                                                DM.constructorHook
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x15]))
                                                                  (Prelude.return (O x15))
                                                      x17 <- Prelude.return x9
                                                      DM.funcCallHook "+^"
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x16, DI.genTerm x17]))
                                                        (op_PlusAccent x16 x17)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x18]))
                                              (Prelude.return (Pos x18))
                                  x20 <- Prelude.return x8
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x19, DI.genTerm x20]))
                                    (Prelude.return (Tuple2 x19 x20))))
                    Neg x10
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x21])))
                           (strict__case_33 x5 x8)
                           x21)))
term_strict__case_33 x1
  = DI.Term "_case_33" (DI.SrcID "Prelude" 0) x1
strict__case_41 x1
  = DM.eval
      (DM.funcDeclHook "_case_41"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x6 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return IHi)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4]))
                                    (Prelude.return (Pos x4))))
                    O x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Zero)))
                    I x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return IHi)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5]))
                                    (Prelude.return (Pos x5))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           strict__case_41
                           x6)))
term_strict__case_41 x1
  = DI.Term "_case_41" (DI.SrcID "Prelude" 0) x1
strict__case_42 x1
  = DM.eval
      (DM.funcDeclHook "_case_42"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    O x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    I x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_42
                           x4)))
term_strict__case_42 x1
  = DI.Term "_case_42" (DI.SrcID "Prelude" 0) x1
strict__case_45 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_45"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x13 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x7 <- Prelude.return x2
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7]))
                                             (Prelude.return (Neg x7))
                                  DM.funcCallHook "inc"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (strict_inc x8)))
                    O x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x3
                                  x10 <- Prelude.return x2
                                  DM.funcCallHook "_case_44"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_44 x9 x10)))
                    I x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x6
                                  x12 <- Prelude.return x2
                                  DM.funcCallHook "_case_43"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (strict__case_43 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_45 x2)
                           x13)))
term_strict__case_45 x1
  = DI.Term "_case_45" (DI.SrcID "Prelude" 0) x1
strict__case_43 x6 x2
  = DM.eval
      (DM.funcDeclHook "_case_43"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x2]))
         (do x18 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x18]))
               (case x18 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- do x9 <- Prelude.return x6
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x9]))
                                              (Prelude.return (O x9))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (Prelude.return (Pos x10))))
                    O x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- do x13 <- do x11 <- Prelude.return x6
                                                      x12 <- Prelude.return x7
                                                      DM.funcCallHook "-^"
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x11, DI.genTerm x12]))
                                                        (op_MinusAccent x11 x12)
                                            DM.funcCallHook "mult2"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x13]))
                                              (strict_mult2 x13)
                                  DM.funcCallHook "inc"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14]))
                                    (strict_inc x14)))
                    I x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x17 <- do x15 <- Prelude.return x6
                                            x16 <- Prelude.return x8
                                            DM.funcCallHook "-^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x15, DI.genTerm x16]))
                                              (op_MinusAccent x15 x16)
                                  DM.funcCallHook "mult2"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x17]))
                                    (strict_mult2 x17)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x18])))
                           (strict__case_43 x6)
                           x18)))
term_strict__case_43 x1
  = DI.Term "_case_43" (DI.SrcID "Prelude" 0) x1
strict__case_44 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_44"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x2]))
         (do x16 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x7 <- do x6 <- Prelude.return x3
                                                    DM.constructorHook
                                                      (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x6]))
                                                      (Prelude.return (O x6))
                                           DM.funcCallHook "pred"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7]))
                                             (strict_pred x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (Prelude.return (Pos x8))))
                    O x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x9 <- Prelude.return x3
                                            x10 <- Prelude.return x4
                                            DM.funcCallHook "-^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (op_MinusAccent x9 x10)
                                  DM.funcCallHook "mult2"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11]))
                                    (strict_mult2 x11)))
                    I x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x15 <- do x14 <- do x12 <- Prelude.return x3
                                                      x13 <- Prelude.return x5
                                                      DM.funcCallHook "-^"
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x12, DI.genTerm x13]))
                                                        (op_MinusAccent x12 x13)
                                            DM.funcCallHook "mult2"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x14]))
                                              (strict_mult2 x14)
                                  DM.funcCallHook "dec"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x15]))
                                    (strict_dec x15)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_44 x3)
                           x16)))
term_strict__case_44 x1
  = DI.Term "_case_44" (DI.SrcID "Prelude" 0) x1
strict__case_46 x1
  = DM.eval
      (DM.funcDeclHook "_case_46"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Pos x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- do x4 <- Prelude.return x2
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4]))
                                             (Prelude.return (O x4))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5]))
                                    (Prelude.return (Pos x5))))
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Zero)))
                    Neg x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- do x6 <- Prelude.return x3
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6]))
                                             (Prelude.return (O x6))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7]))
                                    (Prelude.return (Neg x7))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_46
                           x8)))
term_strict__case_46 x1
  = DI.Term "_case_46" (DI.SrcID "Prelude" 0) x1
strict__case_48 x1
  = DM.eval
      (DM.funcDeclHook "_case_48"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return IHi)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4]))
                                    (Prelude.return (Neg x4))))
                    Neg x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- do x5 <- Prelude.return x2
                                           DM.funcCallHook "succ"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (strict_succ x5)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Prelude.return (Neg x6))))
                    Pos x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  DM.funcCallHook "_case_47"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7]))
                                    (strict__case_47 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_48
                           x8)))
term_strict__case_48 x1
  = DI.Term "_case_48" (DI.SrcID "Prelude" 0) x1
strict__case_47 x3
  = DM.eval
      (DM.funcDeclHook "_case_47"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3]))
         (do x11 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Zero)))
                    O x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x7 <- do x6 <- Prelude.return x4
                                                    DM.constructorHook
                                                      (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x6]))
                                                      (Prelude.return (O x6))
                                           DM.funcCallHook "pred"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7]))
                                             (strict_pred x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (Prelude.return (Pos x8))))
                    I x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- do x9 <- Prelude.return x5
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x9]))
                                              (Prelude.return (O x9))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (Prelude.return (Pos x10))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           strict__case_47
                           x11)))
term_strict__case_47 x1
  = DI.Term "_case_47" (DI.SrcID "Prelude" 0) x1
strict__case_50 x1
  = DM.eval
      (DM.funcDeclHook "_case_50"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return IHi)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4]))
                                    (Prelude.return (Pos x4))))
                    Pos x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- do x5 <- Prelude.return x2
                                           DM.funcCallHook "succ"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (strict_succ x5)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Prelude.return (Pos x6))))
                    Neg x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  DM.funcCallHook "_case_49"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7]))
                                    (strict__case_49 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_50
                           x8)))
term_strict__case_50 x1
  = DI.Term "_case_50" (DI.SrcID "Prelude" 0) x1
strict__case_49 x3
  = DM.eval
      (DM.funcDeclHook "_case_49"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3]))
         (do x11 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Zero)))
                    O x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x7 <- do x6 <- Prelude.return x4
                                                    DM.constructorHook
                                                      (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x6]))
                                                      (Prelude.return (O x6))
                                           DM.funcCallHook "pred"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7]))
                                             (strict_pred x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (Prelude.return (Neg x8))))
                    I x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- do x9 <- Prelude.return x5
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x9]))
                                              (Prelude.return (O x9))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (Prelude.return (Neg x10))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           strict__case_49
                           x11)))
term_strict__case_49 x1
  = DI.Term "_case_49" (DI.SrcID "Prelude" 0) x1
strict__case_52 x1
  = DM.eval
      (DM.funcDeclHook "_case_52"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    O x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  DM.funcCallHook "_case_51"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (strict__case_51 x6)))
                    I x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x5
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7]))
                                    (Prelude.return (O x7))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_52
                           x8)))
term_strict__case_52 x1
  = DI.Term "_case_52" (DI.SrcID "Prelude" 0) x1
strict__case_51 x2
  = DM.eval
      (DM.funcDeclHook "_case_51"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2]))
         (do x9 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return IHi)))
                    O x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- do x5 <- Prelude.return x2
                                           DM.funcCallHook "pred"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (strict_pred x5)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Prelude.return (I x6))))
                    I x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x7 <- Prelude.return x4
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7]))
                                             (Prelude.return (O x7))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (Prelude.return (I x8))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           strict__case_51
                           x9)))
term_strict__case_51 x1
  = DI.Term "_case_51" (DI.SrcID "Prelude" 0) x1
strict__case_53 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_53"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x13 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    I x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x7 <- do x5 <- Prelude.return x2
                                                    x6 <- Prelude.return x3
                                                    DM.funcCallHook "*^"
                                                      (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x5, DI.genTerm x6]))
                                                      (op_AsteriskAccent x5 x6)
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7]))
                                             (Prelude.return (O x7))
                                  x9 <- Prelude.return x2
                                  DM.funcCallHook "+^"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (op_PlusAccent x8 x9)))
                    O x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- do x10 <- Prelude.return x4
                                            x11 <- Prelude.return x2
                                            DM.funcCallHook "*^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (op_AsteriskAccent x10 x11)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12]))
                                    (Prelude.return (O x12))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_53 x2)
                           x13)))
term_strict__case_53 x1
  = DI.Term "_case_53" (DI.SrcID "Prelude" 0) x1
strict__case_56 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_56"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x11 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return GT)))
                    O x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  x8 <- Prelude.return x1
                                  DM.funcCallHook "_case_55"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (strict__case_55 x7 x8)))
                    I x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x6
                                  x10 <- Prelude.return x1
                                  DM.funcCallHook "_case_54"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_54 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_56 x1)
                           x11)))
term_strict__case_56 x1
  = DI.Term "_case_56" (DI.SrcID "Prelude" 0) x1
strict__case_54 x6 x1
  = DM.eval
      (DM.funcDeclHook "_case_54"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x1]))
         (do x13 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return LT)))
                    I x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x7
                                  x10 <- Prelude.return x6
                                  DM.funcCallHook "cmpNatGT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (strict_cmpNatGT x9 x10)))
                    O x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x8
                                  x12 <- Prelude.return x6
                                  DM.funcCallHook "cmpNatLT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (strict_cmpNatLT x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_54 x6)
                           x13)))
term_strict__case_54 x1
  = DI.Term "_case_54" (DI.SrcID "Prelude" 0) x1
strict__case_55 x3 x1
  = DM.eval
      (DM.funcDeclHook "_case_55"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return LT)))
                    O x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x4
                                  x7 <- Prelude.return x3
                                  DM.funcCallHook "cmpNatGT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict_cmpNatGT x6 x7)))
                    I x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x5
                                  x9 <- Prelude.return x3
                                  DM.funcCallHook "cmpNatGT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (strict_cmpNatGT x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_55 x3)
                           x10)))
term_strict__case_55 x1
  = DI.Term "_case_55" (DI.SrcID "Prelude" 0) x1
strict__case_59 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_59"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x11 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return LT)))
                    O x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  x8 <- Prelude.return x2
                                  DM.funcCallHook "_case_58"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (strict__case_58 x7 x8)))
                    I x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x6
                                  x10 <- Prelude.return x2
                                  DM.funcCallHook "_case_57"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_57 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_59 x2)
                           x11)))
term_strict__case_59 x1
  = DI.Term "_case_59" (DI.SrcID "Prelude" 0) x1
strict__case_57 x6 x2
  = DM.eval
      (DM.funcDeclHook "_case_57"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return GT)))
                    I x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x6
                                  x10 <- Prelude.return x7
                                  DM.funcCallHook "cmpNatLT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (strict_cmpNatLT x9 x10)))
                    O x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x6
                                  x12 <- Prelude.return x8
                                  DM.funcCallHook "cmpNatGT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (strict_cmpNatGT x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_57 x6)
                           x13)))
term_strict__case_57 x1
  = DI.Term "_case_57" (DI.SrcID "Prelude" 0) x1
strict__case_58 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_58"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x2]))
         (do x10 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return GT)))
                    O x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x3
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "cmpNatLT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict_cmpNatLT x6 x7)))
                    I x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x3
                                  x9 <- Prelude.return x5
                                  DM.funcCallHook "cmpNatLT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (strict_cmpNatLT x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_58 x3)
                           x10)))
term_strict__case_58 x1
  = DI.Term "_case_58" (DI.SrcID "Prelude" 0) x1
strict__case_63 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_63"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x14 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x2
                                  DM.funcCallHook "_case_62"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9]))
                                    (strict__case_62 x9)))
                    O x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x5
                                  x11 <- Prelude.return x2
                                  DM.funcCallHook "_case_61"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (strict__case_61 x10 x11)))
                    I x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x8
                                  x13 <- Prelude.return x2
                                  DM.funcCallHook "_case_60"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (strict__case_60 x12 x13)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_63 x2)
                           x14)))
term_strict__case_63 x1
  = DI.Term "_case_63" (DI.SrcID "Prelude" 0) x1
strict__case_60 x8 x2
  = DM.eval
      (DM.funcDeclHook "_case_60"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x2]))
         (do x15 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return GT)))
                    I x9
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x8
                                  x12 <- Prelude.return x9
                                  DM.funcCallHook "cmpNat"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (strict_cmpNat x11 x12)))
                    O x10
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- Prelude.return x8
                                  x14 <- Prelude.return x10
                                  DM.funcCallHook "cmpNatGT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x13, DI.genTerm x14]))
                                    (strict_cmpNatGT x13 x14)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_60 x8)
                           x15)))
term_strict__case_60 x1
  = DI.Term "_case_60" (DI.SrcID "Prelude" 0) x1
strict__case_61 x5 x2
  = DM.eval
      (DM.funcDeclHook "_case_61"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x2]))
         (do x12 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return GT)))
                    O x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x5
                                  x9 <- Prelude.return x6
                                  DM.funcCallHook "cmpNat"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (strict_cmpNat x8 x9)))
                    I x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x5
                                  x11 <- Prelude.return x7
                                  DM.funcCallHook "cmpNatLT"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (strict_cmpNatLT x10 x11)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_61 x5)
                           x12)))
term_strict__case_61 x1
  = DI.Term "_case_61" (DI.SrcID "Prelude" 0) x1
strict__case_62 x2
  = DM.eval
      (DM.funcDeclHook "_case_62"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2]))
         (do x5 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return EQ)))
                    O x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return LT)))
                    I x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return LT)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           strict__case_62
                           x5)))
term_strict__case_62 x1
  = DI.Term "_case_62" (DI.SrcID "Prelude" 0) x1
strict__case_66 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_66"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x12 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    O x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  x8 <- Prelude.return x2
                                  DM.funcCallHook "_case_65"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (strict__case_65 x7 x8)))
                    I x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x6
                                  x10 <- Prelude.return x2
                                  DM.funcCallHook "_case_64"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_64 x9 x10)))
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x2
                                  DM.funcCallHook "succ"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11]))
                                    (strict_succ x11)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_66 x2)
                           x12)))
term_strict__case_66 x1
  = DI.Term "_case_66" (DI.SrcID "Prelude" 0) x1
strict__case_64 x6 x2
  = DM.eval
      (DM.funcDeclHook "_case_64"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x2]))
         (do x18 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x18]))
               (case x18 of
                    O x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x9 <- Prelude.return x6
                                            x10 <- Prelude.return x7
                                            DM.funcCallHook "+^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (op_PlusAccent x9 x10)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11]))
                                    (Prelude.return (I x11))))
                    I x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x15 <- do x13 <- do x12 <- Prelude.return x6
                                                      DM.funcCallHook "succ"
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x12]))
                                                        (strict_succ x12)
                                            x14 <- Prelude.return x8
                                            DM.funcCallHook "+^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x13, DI.genTerm x14]))
                                              (op_PlusAccent x13 x14)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x15]))
                                    (Prelude.return (O x15))))
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x17 <- do x16 <- Prelude.return x6
                                            DM.funcCallHook "succ"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x16]))
                                              (strict_succ x16)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x17]))
                                    (Prelude.return (O x17))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x18])))
                           (strict__case_64 x6)
                           x18)))
term_strict__case_64 x1
  = DI.Term "_case_64" (DI.SrcID "Prelude" 0) x1
strict__case_65 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_65"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    O x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x6 <- Prelude.return x3
                                           x7 <- Prelude.return x4
                                           DM.funcCallHook "+^"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (op_PlusAccent x6 x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (Prelude.return (O x8))))
                    I x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x9 <- Prelude.return x3
                                            x10 <- Prelude.return x5
                                            DM.funcCallHook "+^"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (op_PlusAccent x9 x10)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11]))
                                    (Prelude.return (I x11))))
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x3
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12]))
                                    (Prelude.return (I x12))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_65 x3)
                           x13)))
term_strict__case_65 x1
  = DI.Term "_case_65" (DI.SrcID "Prelude" 0) x1
strict__case_67 x1
  = DM.eval
      (DM.funcDeclHook "_case_67"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    O x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x2
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4]))
                                    (Prelude.return (I x4))))
                    I x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- do x5 <- Prelude.return x3
                                           DM.funcCallHook "succ"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (strict_succ x5)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Prelude.return (O x6))))
                    IHi
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return IHi)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7]))
                                    (Prelude.return (O x7))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_67
                           x8)))
term_strict__case_67 x1
  = DI.Term "_case_67" (DI.SrcID "Prelude" 0) x1
strict__case_69 x1 x2 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_69"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
                DI.genTerm x5]))
         (do x11 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x4
                                  x7 <- Prelude.return x1
                                  DM.funcCallHook "<="
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (op_LtEq x6 x7)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x1
                                  x9 <- Prelude.return x4
                                  x10 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                              (DI.DynamicInfo [] []))
                                           strict_otherwise
                                  DM.funcCallHook "_case_68"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_68 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_69 x1 x2 x3 x4)
                           x11)))
term_strict__case_69 x1
  = DI.Term "_case_69" (DI.SrcID "Prelude" 0) x1
strict__case_68 x1 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_68"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5]))
         (do x8 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x4
                                  x7 <- Prelude.return x1
                                  DM.funcCallHook ">="
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (op_GtEq x6 x7)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_68 x1 x4)
                           x8)))
term_strict__case_68 x1
  = DI.Term "_case_68" (DI.SrcID "Prelude" 0) x1
strict__case_70 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_70"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x10 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x1
                                  x9 <- do x6 <- do x4 <- Prelude.return x1
                                                    x5 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return (Pos IHi))
                                                    DM.funcCallHook "+"
                                                      (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x4, DI.genTerm x5]))
                                                      (op_Plus x4 x5)
                                           x7 <- Prelude.return x2
                                           DM.funcCallHook "enumFromTo"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (strict_enumFromTo x6 x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Cons x8 x9))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_70 x1 x2)
                           x10)))
term_strict__case_70 x1
  = DI.Term "_case_70" (DI.SrcID "Prelude" 0) x1
strict__case_74 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_74"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nothing)))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- Prelude.return x4
                                  x7 <- Prelude.return x3
                                  DM.funcCallHook "_case_73"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_73 x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_74 x1)
                           x8)))
term_strict__case_74 x1
  = DI.Term "_case_74" (DI.SrcID "Prelude" 0) x1
strict__case_73 x1 x4 x3
  = DM.eval
      (DM.funcDeclHook "_case_73"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x4, DI.genTerm x3]))
         (do x14 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Tuple2 x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- Prelude.return x4
                                  x11 <- Prelude.return x5
                                  x12 <- Prelude.return x6
                                  x13 <- do x7 <- Prelude.return x1
                                            x8 <- Prelude.return x5
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                              (op_EqEq x7 x8)
                                  DM.funcCallHook "_case_72"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11,
                                           DI.genTerm x12, DI.genTerm x13]))
                                    (strict__case_72 x9 x10 x11 x12 x13)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_73 x1 x4)
                           x14)))
term_strict__case_73 x1
  = DI.Term "_case_73" (DI.SrcID "Prelude" 0) x1
strict__case_72 x1 x4 x5 x6 x7
  = DM.eval
      (DM.funcDeclHook "_case_72"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6,
                DI.genTerm x7]))
         (do x12 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x6
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (Prelude.return (Just x8))))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- Prelude.return x4
                                  x11 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                              (DI.DynamicInfo [] []))
                                           strict_otherwise
                                  DM.funcCallHook "_case_71"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11]))
                                    (strict__case_71 x9 x10 x11)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_72 x1 x4 x5 x6)
                           x12)))
term_strict__case_72 x1
  = DI.Term "_case_72" (DI.SrcID "Prelude" 0) x1
strict__case_71 x1 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_71"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5]))
         (do x8 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "lookup"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict_lookup x6 x7)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_71 x1 x4)
                           x8)))
term_strict__case_71 x1
  = DI.Term "_case_71" (DI.SrcID "Prelude" 0) x1
strict__case_75 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_75"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- Prelude.return
                                          (PC.partCall2 (x'xterm_strict_unwords46_35lambda6 [])
                                             x'xstrict_unwords46_35lambda6)
                                  x4 <- Prelude.return x1
                                  DM.funcCallHook "foldr1"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                    (strict_foldr1 x3 x4)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           (strict__case_75 x1)
                           x5)))
term_strict__case_75 x1
  = DI.Term "_case_75" (DI.SrcID "Prelude" 0) x1
strict__case_76 x1
  = DM.eval
      (DM.funcDeclHook "_case_76"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_76
                           x4)))
term_strict__case_76 x1
  = DI.Term "_case_76" (DI.SrcID "Prelude" 0) x1
strict__case_77 x1
  = DM.eval
      (DM.funcDeclHook "_case_77"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_77
                           x4)))
term_strict__case_77 x1
  = DI.Term "_case_77" (DI.SrcID "Prelude" 0) x1
strict__case_78 x2 x6
  = DM.eval
      (DM.funcDeclHook "_case_78"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x6]))
         (do x15 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x3 <- do x8 <- do x7 <- Prelude.return
                                                               (PC.partCall1
                                                                  (x'xterm_strict_words46isSpace46326
                                                                     [])
                                                                  x'xstrict_words46isSpace46326)
                                                       DM.funcCallHook "break"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x7]))
                                                         (strict_break x7)
                                              x9 <- Prelude.return x2
                                              DM.funcCallHook "apply"
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x8, DI.genTerm x9]))
                                                (strict_apply x8 x9)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x4 <- do x10 <- Prelude.return x3
                                                       DM.funcCallHook "words._#selFP22#w"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x10]))
                                                         (x'xstrict_words46_35selFP2235w x10)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x5 <- do x11 <- Prelude.return x3
                                                                DM.funcCallHook "words._#selFP23#s2"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x11]))
                                                                  (x'xstrict_words46_35selFP2335s2
                                                                     x11)
                                                       DM.eval
                                                         (do x13 <- Prelude.return x4
                                                             x14 <- do x12 <- Prelude.return x5
                                                                       DM.funcCallHook "words"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x12]))
                                                                         (strict_words x12)
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x13,
                                                                      DI.genTerm x14]))
                                                               (Prelude.return
                                                                  (Cons x13 x14))))))))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_78 x2)
                           x15)))
term_strict__case_78 x1
  = DI.Term "_case_78" (DI.SrcID "Prelude" 0) x1
strict__case_79 x1
  = DM.eval
      (DM.funcDeclHook "_case_79"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_79
                           x4)))
term_strict__case_79 x1
  = DI.Term "_case_79" (DI.SrcID "Prelude" 0) x1
strict__case_80 x1
  = DM.eval
      (DM.funcDeclHook "_case_80"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_80
                           x4)))
term_strict__case_80 x1
  = DI.Term "_case_80" (DI.SrcID "Prelude" 0) x1
strict__case_81 x1
  = DM.eval
      (DM.funcDeclHook "_case_81"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_81
                           x4)))
term_strict__case_81 x1
  = DI.Term "_case_81" (DI.SrcID "Prelude" 0) x1
strict__case_82 x1
  = DM.eval
      (DM.funcDeclHook "_case_82"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_82
                           x4)))
term_strict__case_82 x1
  = DI.Term "_case_82" (DI.SrcID "Prelude" 0) x1
strict__case_84 x1
  = DM.eval
      (DM.funcDeclHook "_case_84"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x11 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  x5 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (Prelude.return (Tuple2 x4 x5))))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x2
                                  x9 <- Prelude.return x3
                                  x10 <- do x6 <- Prelude.return x2
                                            x7 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return (Char '\n'))
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (op_EqEq x6 x7)
                                  DM.funcCallHook "_case_83"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_83 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           strict__case_84
                           x11)))
term_strict__case_84 x1
  = DI.Term "_case_84" (DI.SrcID "Prelude" 0) x1
strict__case_83 x2 x3 x7
  = DM.eval
      (DM.funcDeclHook "_case_83"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3, DI.genTerm x7]))
         (do x17 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x17]))
               (case x17 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  x9 <- Prelude.return x3
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Tuple2 x8 x9))))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x4 <- do x10 <- Prelude.return x3
                                              DM.funcCallHook "lines.splitline.314"
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo [] [DI.genTerm x10]))
                                                (x'xstrict_lines46splitline46314 x10)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x5 <- do x11 <- Prelude.return x4
                                                       DM.funcCallHook
                                                         "lines.splitline.314._#selFP16#ds"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x11]))
                                                         (x'xstrict_lines46splitline4631446_35selFP1635ds
                                                            x11)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x6 <- do x12 <- Prelude.return x4
                                                                DM.funcCallHook
                                                                  "lines.splitline.314._#selFP17#es"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x12]))
                                                                  (x'xstrict_lines46splitline4631446_35selFP1735es
                                                                     x12)
                                                       DM.eval
                                                         (do x15 <- do x13 <- Prelude.return x2
                                                                       x14 <- Prelude.return x5
                                                                       DM.constructorHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x13,
                                                                                DI.genTerm x14]))
                                                                         (Prelude.return
                                                                            (Cons x13 x14))
                                                             x16 <- Prelude.return x6
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x15,
                                                                      DI.genTerm x16]))
                                                               (Prelude.return
                                                                  (Tuple2 x15 x16))))))))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x17])))
                           (strict__case_83 x2 x3)
                           x17)))
term_strict__case_83 x1
  = DI.Term "_case_83" (DI.SrcID "Prelude" 0) x1
strict__case_85 x1
  = DM.eval
      (DM.funcDeclHook "_case_85"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x15 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x4 <- do x9 <- do x7 <- Prelude.return x2
                                                       x8 <- Prelude.return x3
                                                       DM.constructorHook
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo []
                                                               [DI.genTerm x7, DI.genTerm x8]))
                                                         (Prelude.return (Cons x7 x8))
                                              DM.funcCallHook "lines.splitline.314"
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo [] [DI.genTerm x9]))
                                                (x'xstrict_lines46splitline46314 x9)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x5 <- do x10 <- Prelude.return x4
                                                       DM.funcCallHook "lines._#selFP19#l"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x10]))
                                                         (x'xstrict_lines46_35selFP1935l x10)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x6 <- do x11 <- Prelude.return x4
                                                                DM.funcCallHook
                                                                  "lines._#selFP20#xs_l"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x11]))
                                                                  (x'xstrict_lines46_35selFP2035xs_l
                                                                     x11)
                                                       DM.eval
                                                         (do x13 <- Prelude.return x5
                                                             x14 <- do x12 <- Prelude.return x6
                                                                       DM.funcCallHook "lines"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x12]))
                                                                         (strict_lines x12)
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x13,
                                                                      DI.genTerm x14]))
                                                               (Prelude.return
                                                                  (Cons x13 x14))))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           strict__case_85
                           x15)))
term_strict__case_85 x1
  = DI.Term "_case_85" (DI.SrcID "Prelude" 0) x1
strict__case_86 x1
  = DM.eval
      (DM.funcDeclHook "_case_86"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_86
                           x4)))
term_strict__case_86 x1
  = DI.Term "_case_86" (DI.SrcID "Prelude" 0) x1
strict__case_87 x1
  = DM.eval
      (DM.funcDeclHook "_case_87"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_87
                           x4)))
term_strict__case_87 x1
  = DI.Term "_case_87" (DI.SrcID "Prelude" 0) x1
strict__case_90 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_90"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  x6 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (Prelude.return (Tuple2 x5 x6))))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- Prelude.return x3
                                  x11 <- Prelude.return x4
                                  x12 <- do x7 <- Prelude.return x1
                                            x8 <- Prelude.return x3
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                              (strict_apply x7 x8)
                                  DM.funcCallHook "_case_89"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11,
                                           DI.genTerm x12]))
                                    (strict__case_89 x9 x10 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_90 x1)
                           x13)))
term_strict__case_90 x1
  = DI.Term "_case_90" (DI.SrcID "Prelude" 0) x1
strict__case_89 x1 x3 x4 x8
  = DM.eval
      (DM.funcDeclHook "_case_89"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4, DI.genTerm x8]))
         (do x20 <- Prelude.return x8
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x5 <- do x9 <- Prelude.return x1
                                              x10 <- Prelude.return x4
                                              DM.funcCallHook "span"
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x9, DI.genTerm x10]))
                                                (strict_span x9 x10)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x6 <- do x11 <- Prelude.return x5
                                                       DM.funcCallHook "span._#selFP13#ys"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x11]))
                                                         (x'xstrict_span46_35selFP1335ys x11)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x7 <- do x12 <- Prelude.return x5
                                                                DM.funcCallHook "span._#selFP14#zs"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x12]))
                                                                  (x'xstrict_span46_35selFP1435zs
                                                                     x12)
                                                       DM.eval
                                                         (do x15 <- do x13 <- Prelude.return x3
                                                                       x14 <- Prelude.return x6
                                                                       DM.constructorHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x13,
                                                                                DI.genTerm x14]))
                                                                         (Prelude.return
                                                                            (Cons x13 x14))
                                                             x16 <- Prelude.return x7
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x15,
                                                                      DI.genTerm x16]))
                                                               (Prelude.return
                                                                  (Tuple2 x15 x16))))))))))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x17 <- Prelude.return x3
                                  x18 <- Prelude.return x4
                                  x19 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                              (DI.DynamicInfo [] []))
                                           strict_otherwise
                                  DM.funcCallHook "_case_88"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x17, DI.genTerm x18, DI.genTerm x19]))
                                    (strict__case_88 x17 x18 x19)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           (strict__case_89 x1 x3 x4)
                           x20)))
term_strict__case_89 x1
  = DI.Term "_case_89" (DI.SrcID "Prelude" 0) x1
strict__case_88 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_88"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x10 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  x9 <- do x6 <- Prelude.return x3
                                           x7 <- Prelude.return x4
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (Prelude.return (Cons x6 x7))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Tuple2 x8 x9))))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_88 x3 x4)
                           x10)))
term_strict__case_88 x1
  = DI.Term "_case_88" (DI.SrcID "Prelude" 0) x1
strict__case_92 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_92"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x11 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x3
                                  x9 <- Prelude.return x4
                                  x10 <- do x5 <- Prelude.return x1
                                            x6 <- Prelude.return x3
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                              (strict_apply x5 x6)
                                  DM.funcCallHook "_case_91"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9,
                                           DI.genTerm x10]))
                                    (strict__case_91 x7 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_92 x1)
                           x11)))
term_strict__case_92 x1
  = DI.Term "_case_92" (DI.SrcID "Prelude" 0) x1
strict__case_91 x1 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_91"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x10 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "dropWhile"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict_dropWhile x6 x7)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x3
                                  x9 <- Prelude.return x4
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Cons x8 x9))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_91 x1 x3 x4)
                           x10)))
term_strict__case_91 x1
  = DI.Term "_case_91" (DI.SrcID "Prelude" 0) x1
strict__case_94 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_94"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x11 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x3
                                  x9 <- Prelude.return x4
                                  x10 <- do x5 <- Prelude.return x1
                                            x6 <- Prelude.return x3
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                              (strict_apply x5 x6)
                                  DM.funcCallHook "_case_93"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9,
                                           DI.genTerm x10]))
                                    (strict__case_93 x7 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_94 x1)
                           x11)))
term_strict__case_94 x1
  = DI.Term "_case_94" (DI.SrcID "Prelude" 0) x1
strict__case_93 x1 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_93"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x10 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x3
                                  x9 <- do x6 <- Prelude.return x1
                                           x7 <- Prelude.return x4
                                           DM.funcCallHook "takeWhile"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (strict_takeWhile x6 x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Cons x8 x9))))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_93 x1 x3 x4)
                           x10)))
term_strict__case_93 x1
  = DI.Term "_case_93" (DI.SrcID "Prelude" 0) x1
strict__case_95 x1
  = DM.eval
      (DM.funcDeclHook "_case_95"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_95
                           x4)))
term_strict__case_95 x1
  = DI.Term "_case_95" (DI.SrcID "Prelude" 0) x1
strict__case_96 x1
  = DM.eval
      (DM.funcDeclHook "_case_96"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_96
                           x4)))
term_strict__case_96 x1
  = DI.Term "_case_96" (DI.SrcID "Prelude" 0) x1
strict__case_97 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_97"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x20 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  x9 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Tuple2 x8 x9))))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x5 <- do x12 <- do x10 <- Prelude.return x1
                                                        x11 <- DM.litHook
                                                                 (DI.DebugInfo
                                                                    (DI.SrcID "Prelude" 0)
                                                                    (DI.DynamicInfo [] []))
                                                                 (Prelude.return (Pos IHi))
                                                        DM.funcCallHook "-"
                                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                             (DI.DynamicInfo []
                                                                [DI.genTerm x10, DI.genTerm x11]))
                                                          (op_Minus x10 x11)
                                              x13 <- Prelude.return x4
                                              DM.funcCallHook "splitAt"
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x12, DI.genTerm x13]))
                                                (strict_splitAt x12 x13)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x6 <- do x14 <- Prelude.return x5
                                                       DM.funcCallHook
                                                         "splitAt.splitAtp.282._#selFP10#ys"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x14]))
                                                         (x'xstrict_splitAt46splitAtp4628246_35selFP1035ys
                                                            x14)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x7 <- do x15 <- Prelude.return x5
                                                                DM.funcCallHook
                                                                  "splitAt.splitAtp.282._#selFP11#zs"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x15]))
                                                                  (x'xstrict_splitAt46splitAtp4628246_35selFP1135zs
                                                                     x15)
                                                       DM.eval
                                                         (do x18 <- do x16 <- Prelude.return x3
                                                                       x17 <- Prelude.return x6
                                                                       DM.constructorHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x16,
                                                                                DI.genTerm x17]))
                                                                         (Prelude.return
                                                                            (Cons x16 x17))
                                                             x19 <- Prelude.return x7
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x18,
                                                                      DI.genTerm x19]))
                                                               (Prelude.return
                                                                  (Tuple2 x18 x19))))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           (strict__case_97 x1)
                           x20)))
term_strict__case_97 x1
  = DI.Term "_case_97" (DI.SrcID "Prelude" 0) x1
strict__case_98 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_98"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x8 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  x5 <- Prelude.return x2
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (Prelude.return (Tuple2 x4 x5))))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x2
                                  DM.funcCallHook "splitAt.splitAtp.282"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (x'xstrict_splitAt46splitAtp46282 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_98 x1 x2)
                           x8)))
term_strict__case_98 x1
  = DI.Term "_case_98" (DI.SrcID "Prelude" 0) x1
strict__case_99 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_99"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x9 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- do x5 <- Prelude.return x1
                                           x6 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return (Pos IHi))
                                           DM.funcCallHook "-"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (op_Minus x5 x6)
                                  x8 <- Prelude.return x4
                                  DM.funcCallHook "drop"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (strict_drop x7 x8)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           (strict__case_99 x1)
                           x9)))
term_strict__case_99 x1
  = DI.Term "_case_99" (DI.SrcID "Prelude" 0) x1
strict__case_100 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_100"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x1
                                  x5 <- Prelude.return x2
                                  DM.funcCallHook "drop.dropp.272"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (x'xstrict_drop46dropp46272 x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           (strict__case_100 x1 x2)
                           x6)))
term_strict__case_100 x1
  = DI.Term "_case_100" (DI.SrcID "Prelude" 0) x1
strict__case_102 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_102"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x7 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Neg x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Pos x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x4
                                  x6 <- Prelude.return x2
                                  DM.funcCallHook "_case_101"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (strict__case_101 x5 x6)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_102 x2)
                           x7)))
term_strict__case_102 x1
  = DI.Term "_case_102" (DI.SrcID "Prelude" 0) x1
strict__case_101 x4 x2
  = DM.eval
      (DM.funcDeclHook "_case_101"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x2]))
         (do x14 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x5
                                  x13 <- do x10 <- do x8 <- do x7 <- Prelude.return x4
                                                               DM.constructorHook
                                                                 (DI.DebugInfo
                                                                    (DI.SrcID "Prelude" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x7]))
                                                                 (Prelude.return (Pos x7))
                                                      x9 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return (Pos IHi))
                                                      DM.funcCallHook "-"
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x8, DI.genTerm x9]))
                                                        (op_Minus x8 x9)
                                            x11 <- Prelude.return x6
                                            DM.funcCallHook "take"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (strict_take x10 x11)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (Prelude.return (Cons x12 x13))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_101 x4)
                           x14)))
term_strict__case_101 x1
  = DI.Term "_case_101" (DI.SrcID "Prelude" 0) x1
strict__case_103 x1
  = DM.eval
      (DM.funcDeclHook "_case_103"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x5 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    Tuple3 x2 x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x4))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           strict__case_103
                           x5)))
term_strict__case_103 x1
  = DI.Term "_case_103" (DI.SrcID "Prelude" 0) x1
strict__case_104 x1
  = DM.eval
      (DM.funcDeclHook "_case_104"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x5 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    Tuple3 x2 x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           strict__case_104
                           x5)))
term_strict__case_104 x1
  = DI.Term "_case_104" (DI.SrcID "Prelude" 0) x1
strict__case_105 x1
  = DM.eval
      (DM.funcDeclHook "_case_105"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x5 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    Tuple3 x2 x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           strict__case_105
                           x5)))
term_strict__case_105 x1
  = DI.Term "_case_105" (DI.SrcID "Prelude" 0) x1
strict__case_107 x1
  = DM.eval
      (DM.funcDeclHook "_case_107"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x9 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  x5 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  x6 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
                                    (Prelude.return (Tuple3 x4 x5 x6))))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  x8 <- Prelude.return x2
                                  DM.funcCallHook "_case_106"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (strict__case_106 x7 x8)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           strict__case_107
                           x9)))
term_strict__case_107 x1
  = DI.Term "_case_107" (DI.SrcID "Prelude" 0) x1
strict__case_106 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_106"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x2]))
         (do x24 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x24]))
               (case x24 of
                    Tuple3 x4 x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x7 <- do x11 <- Prelude.return x3
                                              DM.funcCallHook "unzip3"
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo [] [DI.genTerm x11]))
                                                (strict_unzip3 x11)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x8 <- do x12 <- Prelude.return x7
                                                       DM.funcCallHook "unzip3._#selFP6#xs"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x12]))
                                                         (x'xstrict_unzip346_35selFP635xs x12)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x9 <- do x13 <- Prelude.return x7
                                                                DM.funcCallHook "unzip3._#selFP7#ys"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x13]))
                                                                  (x'xstrict_unzip346_35selFP735ys
                                                                     x13)
                                                       DM.eval
                                                         (DM.letHook
                                                            (DI.DebugInfo
                                                               (DI.SrcID "DummyModule" 42)
                                                               (DI.DynamicInfo [] []))
                                                            (do x10 <- do x14 <- Prelude.return x7
                                                                          DM.funcCallHook
                                                                            "unzip3._#selFP8#zs"
                                                                            (DI.DebugInfo
                                                                               (DI.SrcID "Prelude"
                                                                                  0)
                                                                               (DI.DynamicInfo []
                                                                                  [DI.genTerm x14]))
                                                                            (x'xstrict_unzip346_35selFP835zs
                                                                               x14)
                                                                DM.eval
                                                                  (do x21 <- do x15 <- Prelude.return
                                                                                         x4
                                                                                x16 <- Prelude.return
                                                                                         x8
                                                                                DM.constructorHook
                                                                                  (DI.DebugInfo
                                                                                     (DI.SrcID
                                                                                        "Prelude"
                                                                                        0)
                                                                                     (DI.DynamicInfo
                                                                                        []
                                                                                        [DI.genTerm
                                                                                           x15,
                                                                                         DI.genTerm
                                                                                           x16]))
                                                                                  (Prelude.return
                                                                                     (Cons x15 x16))
                                                                      x22 <- do x17 <- Prelude.return
                                                                                         x5
                                                                                x18 <- Prelude.return
                                                                                         x9
                                                                                DM.constructorHook
                                                                                  (DI.DebugInfo
                                                                                     (DI.SrcID
                                                                                        "Prelude"
                                                                                        0)
                                                                                     (DI.DynamicInfo
                                                                                        []
                                                                                        [DI.genTerm
                                                                                           x17,
                                                                                         DI.genTerm
                                                                                           x18]))
                                                                                  (Prelude.return
                                                                                     (Cons x17 x18))
                                                                      x23 <- do x19 <- Prelude.return
                                                                                         x6
                                                                                x20 <- Prelude.return
                                                                                         x10
                                                                                DM.constructorHook
                                                                                  (DI.DebugInfo
                                                                                     (DI.SrcID
                                                                                        "Prelude"
                                                                                        0)
                                                                                     (DI.DynamicInfo
                                                                                        []
                                                                                        [DI.genTerm
                                                                                           x19,
                                                                                         DI.genTerm
                                                                                           x20]))
                                                                                  (Prelude.return
                                                                                     (Cons x19 x20))
                                                                      DM.constructorHook
                                                                        (DI.DebugInfo
                                                                           (DI.SrcID "Prelude" 0)
                                                                           (DI.DynamicInfo []
                                                                              [DI.genTerm x21,
                                                                               DI.genTerm x22,
                                                                               DI.genTerm x23]))
                                                                        (Prelude.return
                                                                           (Tuple3 x21 x22
                                                                              x23))))))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x24])))
                           (strict__case_106 x3)
                           x24)))
term_strict__case_106 x1
  = DI.Term "_case_106" (DI.SrcID "Prelude" 0) x1
strict__case_108 x1
  = DM.eval
      (DM.funcDeclHook "_case_108"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_108
                           x4)))
term_strict__case_108 x1
  = DI.Term "_case_108" (DI.SrcID "Prelude" 0) x1
strict__case_109 x1
  = DM.eval
      (DM.funcDeclHook "_case_109"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_109
                           x4)))
term_strict__case_109 x1
  = DI.Term "_case_109" (DI.SrcID "Prelude" 0) x1
strict__case_111 x1
  = DM.eval
      (DM.funcDeclHook "_case_111"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  x5 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (Prelude.return (Tuple2 x4 x5))))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x3
                                  x7 <- Prelude.return x2
                                  DM.funcCallHook "_case_110"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_110 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_111
                           x8)))
term_strict__case_111 x1
  = DI.Term "_case_111" (DI.SrcID "Prelude" 0) x1
strict__case_110 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_110"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x2]))
         (do x18 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x18]))
               (case x18 of
                    Tuple2 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x6 <- do x9 <- Prelude.return x3
                                              DM.funcCallHook "unzip"
                                                (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                   (DI.DynamicInfo [] [DI.genTerm x9]))
                                                (strict_unzip x9)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x7 <- do x10 <- Prelude.return x6
                                                       DM.funcCallHook "unzip._#selFP3#xs"
                                                         (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x10]))
                                                         (x'xstrict_unzip46_35selFP335xs x10)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x8 <- do x11 <- Prelude.return x6
                                                                DM.funcCallHook "unzip._#selFP4#ys"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x11]))
                                                                  (x'xstrict_unzip46_35selFP435ys
                                                                     x11)
                                                       DM.eval
                                                         (do x16 <- do x12 <- Prelude.return x4
                                                                       x13 <- Prelude.return x7
                                                                       DM.constructorHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x12,
                                                                                DI.genTerm x13]))
                                                                         (Prelude.return
                                                                            (Cons x12 x13))
                                                             x17 <- do x14 <- Prelude.return x5
                                                                       x15 <- Prelude.return x8
                                                                       DM.constructorHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Prelude" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x14,
                                                                                DI.genTerm x15]))
                                                                         (Prelude.return
                                                                            (Cons x14 x15))
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x16,
                                                                      DI.genTerm x17]))
                                                               (Prelude.return
                                                                  (Tuple2 x16 x17))))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x18])))
                           (strict__case_110 x3)
                           x18)))
term_strict__case_110 x1
  = DI.Term "_case_110" (DI.SrcID "Prelude" 0) x1
strict__case_114 x1 x3 x4 x2
  = DM.eval
      (DM.funcDeclHook "_case_114"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4, DI.genTerm x2]))
         (do x12 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x4
                                  x9 <- Prelude.return x5
                                  x10 <- Prelude.return x6
                                  x11 <- Prelude.return x3
                                  DM.funcCallHook "_case_113"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9,
                                           DI.genTerm x10, DI.genTerm x11]))
                                    (strict__case_113 x7 x8 x9 x10 x11)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_114 x1 x3 x4)
                           x12)))
term_strict__case_114 x1
  = DI.Term "_case_114" (DI.SrcID "Prelude" 0) x1
strict__case_113 x1 x4 x5 x6 x3
  = DM.eval
      (DM.funcDeclHook "_case_113"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6,
                DI.genTerm x3]))
         (do x15 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x7 x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- Prelude.return x5
                                  x11 <- Prelude.return x6
                                  x12 <- Prelude.return x7
                                  x13 <- Prelude.return x8
                                  x14 <- Prelude.return x4
                                  DM.funcCallHook "_case_112"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11,
                                           DI.genTerm x12, DI.genTerm x13, DI.genTerm x14]))
                                    (strict__case_112 x9 x10 x11 x12 x13 x14)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_113 x1 x4 x5 x6)
                           x15)))
term_strict__case_113 x1
  = DI.Term "_case_113" (DI.SrcID "Prelude" 0) x1
strict__case_112 x1 x5 x6 x7 x8 x4
  = DM.eval
      (DM.funcDeclHook "_case_112"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x5, DI.genTerm x6, DI.genTerm x7,
                DI.genTerm x8, DI.genTerm x4]))
         (do x23 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x23]))
               (case x23 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x9 x10
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x21 <- do x15 <- do x13 <- do x11 <- Prelude.return x1
                                                                x12 <- Prelude.return x5
                                                                DM.funcCallHook "apply"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Prelude" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x11,
                                                                         DI.genTerm x12]))
                                                                  (strict_apply x11 x12)
                                                      x14 <- Prelude.return x7
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x13, DI.genTerm x14]))
                                                        (strict_apply x13 x14)
                                            x16 <- Prelude.return x9
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x15, DI.genTerm x16]))
                                              (strict_apply x15 x16)
                                  x22 <- do x17 <- Prelude.return x1
                                            x18 <- Prelude.return x6
                                            x19 <- Prelude.return x8
                                            x20 <- Prelude.return x10
                                            DM.funcCallHook "zipWith3"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x17, DI.genTerm x18, DI.genTerm x19,
                                                     DI.genTerm x20]))
                                              (strict_zipWith3 x17 x18 x19 x20)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x21, DI.genTerm x22]))
                                    (Prelude.return (Cons x21 x22))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x23])))
                           (strict__case_112 x1 x5 x6 x7 x8)
                           x23)))
term_strict__case_112 x1
  = DI.Term "_case_112" (DI.SrcID "Prelude" 0) x1
strict__case_116 x1 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_116"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x2]))
         (do x10 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x4
                                  x8 <- Prelude.return x5
                                  x9 <- Prelude.return x3
                                  DM.funcCallHook "_case_115"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (strict__case_115 x6 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_116 x1 x3)
                           x10)))
term_strict__case_116 x1
  = DI.Term "_case_116" (DI.SrcID "Prelude" 0) x1
strict__case_115 x1 x4 x5 x3
  = DM.eval
      (DM.funcDeclHook "_case_115"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x3]))
         (do x17 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x17]))
               (case x17 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x15 <- do x10 <- do x8 <- Prelude.return x1
                                                      x9 <- Prelude.return x4
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x8, DI.genTerm x9]))
                                                        (strict_apply x8 x9)
                                            x11 <- Prelude.return x6
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (strict_apply x10 x11)
                                  x16 <- do x12 <- Prelude.return x1
                                            x13 <- Prelude.return x5
                                            x14 <- Prelude.return x7
                                            DM.funcCallHook "zipWith"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x12, DI.genTerm x13,
                                                     DI.genTerm x14]))
                                              (strict_zipWith x12 x13 x14)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x15, DI.genTerm x16]))
                                    (Prelude.return (Cons x15 x16))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x17])))
                           (strict__case_115 x1 x4 x5)
                           x17)))
term_strict__case_115 x1
  = DI.Term "_case_115" (DI.SrcID "Prelude" 0) x1
strict__case_119 x2 x3 x1
  = DM.eval
      (DM.funcDeclHook "_case_119"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3, DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x3
                                  x7 <- Prelude.return x4
                                  x8 <- Prelude.return x5
                                  x9 <- Prelude.return x2
                                  DM.funcCallHook "_case_118"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (strict__case_118 x6 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_119 x2 x3)
                           x10)))
term_strict__case_119 x1
  = DI.Term "_case_119" (DI.SrcID "Prelude" 0) x1
strict__case_118 x3 x4 x5 x2
  = DM.eval
      (DM.funcDeclHook "_case_118"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x3, DI.genTerm x4, DI.genTerm x5, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x4
                                  x9 <- Prelude.return x5
                                  x10 <- Prelude.return x6
                                  x11 <- Prelude.return x7
                                  x12 <- Prelude.return x3
                                  DM.funcCallHook "_case_117"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10,
                                           DI.genTerm x11, DI.genTerm x12]))
                                    (strict__case_117 x8 x9 x10 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_118 x3 x4 x5)
                           x13)))
term_strict__case_118 x1
  = DI.Term "_case_118" (DI.SrcID "Prelude" 0) x1
strict__case_117 x4 x5 x6 x7 x3
  = DM.eval
      (DM.funcDeclHook "_case_117"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6, DI.genTerm x7,
                DI.genTerm x3]))
         (do x18 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x18]))
               (case x18 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x8 x9
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x16 <- do x10 <- Prelude.return x4
                                            x11 <- Prelude.return x6
                                            x12 <- Prelude.return x8
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11,
                                                     DI.genTerm x12]))
                                              (Prelude.return (Tuple3 x10 x11 x12))
                                  x17 <- do x13 <- Prelude.return x5
                                            x14 <- Prelude.return x7
                                            x15 <- Prelude.return x9
                                            DM.funcCallHook "zip3"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x13, DI.genTerm x14,
                                                     DI.genTerm x15]))
                                              (strict_zip3 x13 x14 x15)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x16, DI.genTerm x17]))
                                    (Prelude.return (Cons x16 x17))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x18])))
                           (strict__case_117 x4 x5 x6 x7)
                           x18)))
term_strict__case_117 x1
  = DI.Term "_case_117" (DI.SrcID "Prelude" 0) x1
strict__case_121 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_121"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x3
                                  x6 <- Prelude.return x4
                                  x7 <- Prelude.return x2
                                  DM.funcCallHook "_case_120"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_120 x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_121 x2)
                           x8)))
term_strict__case_121 x1
  = DI.Term "_case_121" (DI.SrcID "Prelude" 0) x1
strict__case_120 x3 x4 x2
  = DM.eval
      (DM.funcDeclHook "_case_120"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x7 <- Prelude.return x3
                                            x8 <- Prelude.return x5
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                              (Prelude.return (Tuple2 x7 x8))
                                  x12 <- do x9 <- Prelude.return x4
                                            x10 <- Prelude.return x6
                                            DM.funcCallHook "zip"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (strict_zip x9 x10)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (Prelude.return (Cons x11 x12))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_120 x3 x4)
                           x13)))
term_strict__case_120 x1
  = DI.Term "_case_120" (DI.SrcID "Prelude" 0) x1
strict__case_123 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_123"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x11 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x3
                                  x9 <- Prelude.return x4
                                  x10 <- do x5 <- Prelude.return x1
                                            x6 <- Prelude.return x3
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                              (strict_apply x5 x6)
                                  DM.funcCallHook "_case_122"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9,
                                           DI.genTerm x10]))
                                    (strict__case_122 x7 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_123 x1)
                           x11)))
term_strict__case_123 x1
  = DI.Term "_case_123" (DI.SrcID "Prelude" 0) x1
strict__case_122 x1 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_122"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x12 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x3
                                  x9 <- do x6 <- Prelude.return x1
                                           x7 <- Prelude.return x4
                                           DM.funcCallHook "filter"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (strict_filter x6 x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Cons x8 x9))))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x1
                                  x11 <- Prelude.return x4
                                  DM.funcCallHook "filter"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (strict_filter x10 x11)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_122 x1 x3 x4)
                           x12)))
term_strict__case_122 x1
  = DI.Term "_case_122" (DI.SrcID "Prelude" 0) x1
strict__case_125 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_125"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- Prelude.return x3
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "_case_124"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_124 x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_125 x1)
                           x8)))
term_strict__case_125 x1
  = DI.Term "_case_125" (DI.SrcID "Prelude" 0) x1
strict__case_124 x1 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_124"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4]))
         (do x15 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- do x7 <- Prelude.return x1
                                            x8 <- Prelude.return x3
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                              (strict_apply x7 x8)
                                  x14 <- do x11 <- Prelude.return x1
                                            x12 <- do x9 <- Prelude.return x5
                                                      x10 <- Prelude.return x6
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10]))
                                                        (Prelude.return (Cons x9 x10))
                                            DM.funcCallHook "foldr1"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12]))
                                              (strict_foldr1 x11 x12)
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x13, DI.genTerm x14]))
                                    (strict_apply x13 x14)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_124 x1 x3)
                           x15)))
term_strict__case_124 x1
  = DI.Term "_case_124" (DI.SrcID "Prelude" 0) x1
strict__case_126 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_126"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x13 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x6 <- Prelude.return x1
                                            x7 <- Prelude.return x4
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (strict_apply x6 x7)
                                  x12 <- do x8 <- Prelude.return x1
                                            x9 <- Prelude.return x2
                                            x10 <- Prelude.return x5
                                            DM.funcCallHook "foldr"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10]))
                                              (strict_foldr x8 x9 x10)
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (strict_apply x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_126 x1 x2)
                           x13)))
term_strict__case_126 x1
  = DI.Term "_case_126" (DI.SrcID "Prelude" 0) x1
strict__case_127 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_127"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- Prelude.return x3
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "foldl"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict_foldl x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_127 x1)
                           x8)))
term_strict__case_127 x1
  = DI.Term "_case_127" (DI.SrcID "Prelude" 0) x1
strict__case_128 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_128"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x13 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x1
                                  x11 <- do x8 <- do x6 <- Prelude.return x1
                                                     x7 <- Prelude.return x2
                                                     DM.funcCallHook "apply"
                                                       (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x6, DI.genTerm x7]))
                                                       (strict_apply x6 x7)
                                            x9 <- Prelude.return x4
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (strict_apply x8 x9)
                                  x12 <- Prelude.return x5
                                  DM.funcCallHook "foldl"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x10, DI.genTerm x11, DI.genTerm x12]))
                                    (strict_foldl x10 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_128 x1 x2)
                           x13)))
term_strict__case_128 x1
  = DI.Term "_case_128" (DI.SrcID "Prelude" 0) x1
strict__case_129 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_129"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x11 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- do x5 <- Prelude.return x1
                                           x6 <- Prelude.return x3
                                           DM.funcCallHook "apply"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (strict_apply x5 x6)
                                  x10 <- do x7 <- Prelude.return x1
                                            x8 <- Prelude.return x4
                                            DM.funcCallHook "map"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                              (strict_map x7 x8)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (Prelude.return (Cons x9 x10))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_129 x1)
                           x11)))
term_strict__case_129 x1
  = DI.Term "_case_129" (DI.SrcID "Prelude" 0) x1
strict__case_132 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_132"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x11 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x2
                                  x8 <- Prelude.return x3
                                  x9 <- Prelude.return x4
                                  x10 <- do x5 <- Prelude.return x2
                                            x6 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return Zero)
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                              (op_EqEq x5 x6)
                                  DM.funcCallHook "_case_131"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9,
                                           DI.genTerm x10]))
                                    (strict__case_131 x7 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_132 x2)
                           x11)))
term_strict__case_132 x1
  = DI.Term "_case_132" (DI.SrcID "Prelude" 0) x1
strict__case_131 x2 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_131"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x11 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x2
                                  x9 <- Prelude.return x4
                                  x10 <- do x6 <- Prelude.return x2
                                            x7 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return Zero)
                                            DM.funcCallHook ">"
                                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (op_Gt x6 x7)
                                  DM.funcCallHook "_case_130"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_130 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_131 x2 x3 x4)
                           x11)))
term_strict__case_131 x1
  = DI.Term "_case_131" (DI.SrcID "Prelude" 0) x1
strict__case_130 x2 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_130"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x4, DI.genTerm x5]))
         (do x10 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x4
                                  x9 <- do x6 <- Prelude.return x2
                                           x7 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return (Pos IHi))
                                           DM.funcCallHook "-"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (op_Minus x6 x7)
                                  DM.funcCallHook "!!"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (op_EMarkEMark x8 x9)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_130 x2 x4)
                           x10)))
term_strict__case_130 x1
  = DI.Term "_case_130" (DI.SrcID "Prelude" 0) x1
strict__case_133 x1
  = DM.eval
      (DM.funcDeclHook "_case_133"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x7 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Zero)))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return (Pos IHi))
                                  x6 <- do x4 <- Prelude.return x3
                                           DM.funcCallHook "length"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4]))
                                             (strict_length x4)
                                  DM.funcCallHook "+"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (op_Plus x5 x6)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           strict__case_133
                           x7)))
term_strict__case_133 x1
  = DI.Term "_case_133" (DI.SrcID "Prelude" 0) x1
strict__case_134 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_134"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x9 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  x8 <- do x5 <- Prelude.return x4
                                           x6 <- Prelude.return x2
                                           DM.funcCallHook "++"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (op_PlusPlus x5 x6)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Prelude.return (Cons x7 x8))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           (strict__case_134 x2)
                           x9)))
term_strict__case_134 x1
  = DI.Term "_case_134" (DI.SrcID "Prelude" 0) x1
strict__case_135 x1
  = DM.eval
      (DM.funcDeclHook "_case_135"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return True)))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return False)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_135
                           x4)))
term_strict__case_135 x1
  = DI.Term "_case_135" (DI.SrcID "Prelude" 0) x1
strict__case_136 x1
  = DM.eval
      (DM.funcDeclHook "_case_136"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_136
                           x4)))
term_strict__case_136 x1
  = DI.Term "_case_136" (DI.SrcID "Prelude" 0) x1
strict__case_137 x1
  = DM.eval
      (DM.funcDeclHook "_case_137"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_137
                           x4)))
term_strict__case_137 x1
  = DI.Term "_case_137" (DI.SrcID "Prelude" 0) x1
strict__case_138 x1
  = DM.eval
      (DM.funcDeclHook "_case_138"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_138
                           x4)))
term_strict__case_138 x1
  = DI.Term "_case_138" (DI.SrcID "Prelude" 0) x1
strict__case_139 x1
  = DM.eval
      (DM.funcDeclHook "_case_139"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_139
                           x4)))
term_strict__case_139 x1
  = DI.Term "_case_139" (DI.SrcID "Prelude" 0) x1
strict__case_140 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_140"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    GT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    LT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    EQ
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_140 x1 x2)
                           x4)))
term_strict__case_140 x1
  = DI.Term "_case_140" (DI.SrcID "Prelude" 0) x1
strict__case_141 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_141"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    LT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    EQ
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    GT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_141 x1 x2)
                           x4)))
term_strict__case_141 x1
  = DI.Term "_case_141" (DI.SrcID "Prelude" 0) x1
strict__case_145 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_145"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x14 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x2
                                  DM.funcCallHook "_case_144"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9]))
                                    (strict__case_144 x9)))
                    Pos x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x5
                                  x11 <- Prelude.return x2
                                  DM.funcCallHook "_case_143"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (strict__case_143 x10 x11)))
                    Neg x8
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x8
                                  x13 <- Prelude.return x2
                                  DM.funcCallHook "_case_142"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (strict__case_142 x12 x13)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_145 x2)
                           x14)))
term_strict__case_145 x1
  = DI.Term "_case_145" (DI.SrcID "Prelude" 0) x1
strict__case_142 x8 x2
  = DM.eval
      (DM.funcDeclHook "_case_142"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return LT)))
                    Pos x9
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return LT)))
                    Neg x10
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x10
                                  x12 <- Prelude.return x8
                                  DM.funcCallHook "cmpNat"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (strict_cmpNat x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_142 x8)
                           x13)))
term_strict__case_142 x1
  = DI.Term "_case_142" (DI.SrcID "Prelude" 0) x1
strict__case_143 x5 x2
  = DM.eval
      (DM.funcDeclHook "_case_143"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x2]))
         (do x10 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return GT)))
                    Pos x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x5
                                  x9 <- Prelude.return x6
                                  DM.funcCallHook "cmpNat"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (strict_cmpNat x8 x9)))
                    Neg x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return GT)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_143 x5)
                           x10)))
term_strict__case_143 x1
  = DI.Term "_case_143" (DI.SrcID "Prelude" 0) x1
strict__case_144 x2
  = DM.eval
      (DM.funcDeclHook "_case_144"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2]))
         (do x5 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    Zero
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return EQ)))
                    Pos x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return LT)))
                    Neg x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return GT)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           strict__case_144
                           x5)))
term_strict__case_144 x1
  = DI.Term "_case_144" (DI.SrcID "Prelude" 0) x1
strict__case_146 x1
  = DM.eval
      (DM.funcDeclHook "_case_146"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (case x2 of
                    LT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return False)))
                    GT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return False)))
                    EQ
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return True)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x2])))
                           strict__case_146
                           x2)))
term_strict__case_146 x1
  = DI.Term "_case_146" (DI.SrcID "Prelude" 0) x1
strict__case_147 x1
  = DM.eval
      (DM.funcDeclHook "_case_147"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (case x2 of
                    LT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return False)))
                    GT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return True)))
                    EQ
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return False)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x2])))
                           strict__case_147
                           x2)))
term_strict__case_147 x1
  = DI.Term "_case_147" (DI.SrcID "Prelude" 0) x1
strict__case_148 x1
  = DM.eval
      (DM.funcDeclHook "_case_148"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (case x2 of
                    LT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return True)))
                    GT
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return False)))
                    EQ
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return False)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x2])))
                           strict__case_148
                           x2)))
term_strict__case_148 x1
  = DI.Term "_case_148" (DI.SrcID "Prelude" 0) x1
strict__case_149 x2 x3 x1
  = DM.eval
      (DM.funcDeclHook "_case_149"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3, DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_149 x2 x3)
                           x4)))
term_strict__case_149 x1
  = DI.Term "_case_149" (DI.SrcID "Prelude" 0) x1
strict__case_150 x1
  = DM.eval
      (DM.funcDeclHook "_case_150"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (case x2 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return False)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return True)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x2])))
                           strict__case_150
                           x2)))
term_strict__case_150 x1
  = DI.Term "_case_150" (DI.SrcID "Prelude" 0) x1
strict__case_151 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_151"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (case x3 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return True)))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x3])))
                           (strict__case_151 x2)
                           x3)))
term_strict__case_151 x1
  = DI.Term "_case_151" (DI.SrcID "Prelude" 0) x1
strict__case_152 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_152"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (case x3 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return False)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x3])))
                           (strict__case_152 x2)
                           x3)))
term_strict__case_152 x1
  = DI.Term "_case_152" (DI.SrcID "Prelude" 0) x1
strict__case_153 x1
  = DM.eval
      (DM.funcDeclHook "_case_153"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Nil)))
                    Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- do x4 <- DM.funcCallHook "ensureSpine"
                                                   (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                      (DI.DynamicInfo [] []))
                                                   strict_ensureSpine
                                           x5 <- Prelude.return x3
                                           DM.funcCallHook "apply"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (strict_apply x4 x5)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (Prelude.return (Cons x6 x7))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_153
                           x8)))
term_strict__case_153 x1
  = DI.Term "_case_153" (DI.SrcID "Prelude" 0) x1
strict__case_154 x1 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_154"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x10 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x2
                                  x9 <- do x5 <- Prelude.return x2
                                           x6 <- Prelude.return x3
                                           DM.funcCallHook "apply"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (strict_apply x5 x6)
                                  DM.funcCallHook "until"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
                                    (strict_until x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_154 x1 x2 x3)
                           x10)))
term_strict__case_154 x1
  = DI.Term "_case_154" (DI.SrcID "Prelude" 0) x1
strict__case_155 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_155"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x9 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Tuple2 x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- do x5 <- Prelude.return x1
                                           x6 <- Prelude.return x3
                                           DM.funcCallHook "apply"
                                             (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (strict_apply x5 x6)
                                  x8 <- Prelude.return x4
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Prelude" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (strict_apply x7 x8)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Prelude" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           (strict__case_155 x1)
                           x9)))
term_strict__case_155 x1
  = DI.Term "_case_155" (DI.SrcID "Prelude" 0) x1
hook_op_DollarEMark x1 x2 value
  = DM.eval
      (DM.funcDeclHook "$!"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_op_DollarEMark x1 = DI.Term "$!" (DI.SrcID "Prelude" 0) x1
hook_op_DollarEMarkEMark x1 x2 value
  = DM.eval
      (DM.funcDeclHook "$!!"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_op_DollarEMarkEMark x1
  = DI.Term "$!!" (DI.SrcID "Prelude" 0) x1
hook_op_DollarRhomb x1 x2 value
  = DM.eval
      (DM.funcDeclHook "$#"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_op_DollarRhomb x1 = DI.Term "$#" (DI.SrcID "Prelude" 0) x1
hook_op_DollarRhombRhomb x1 x2 value
  = DM.eval
      (DM.funcDeclHook "$##"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_op_DollarRhombRhomb x1
  = DI.Term "$##" (DI.SrcID "Prelude" 0) x1
hook_strict_prim_error x1 value
  = DM.eval
      (DM.funcDeclHook "prim_error"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_error x1
  = DI.Term "prim_error" (DI.SrcID "Prelude" 0) x1
hook_strict_failed value
  = DM.eval
      (DM.funcDeclHook "failed"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         value)
term_strict_failed x1 = DI.Term "failed" (DI.SrcID "Prelude" 0) x1
hook_op_EqEq x1 x2 value
  = DM.eval
      (DM.funcDeclHook "=="
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_op_EqEq x1 = DI.Term "==" (DI.SrcID "Prelude" 0) x1
hook_strict_prim_ord x1 value
  = DM.eval
      (DM.funcDeclHook "prim_ord"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_ord x1
  = DI.Term "prim_ord" (DI.SrcID "Prelude" 0) x1
hook_strict_prim_chr x1 value
  = DM.eval
      (DM.funcDeclHook "prim_chr"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_chr x1
  = DI.Term "prim_chr" (DI.SrcID "Prelude" 0) x1
hook_op_EqEqEq x1 x2 value
  = DM.eval
      (DM.funcDeclHook "==="
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_op_EqEqEq x1 = DI.Term "===" (DI.SrcID "Prelude" 0) x1
hook_op_And x1 x2 value
  = DM.eval
      (DM.funcDeclHook "&"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_op_And x1 = DI.Term "&" (DI.SrcID "Prelude" 0) x1
hook_op_GtGtEq x1 x2 value
  = DM.eval
      (DM.funcDeclHook ">>="
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_op_GtGtEq x1 = DI.Term ">>=" (DI.SrcID "Prelude" 0) x1
hook_strict_return x1 value
  = DM.eval
      (DM.funcDeclHook "return"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_return x1 = DI.Term "return" (DI.SrcID "Prelude" 0) x1
hook_strict_prim_putChar x1 value
  = DM.eval
      (DM.funcDeclHook "prim_putChar"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_putChar x1
  = DI.Term "prim_putChar" (DI.SrcID "Prelude" 0) x1
hook_strict_getChar value
  = DM.eval
      (DM.funcDeclHook "getChar"
         (DI.DebugInfo (DI.SrcID "Prelude" 0) (DI.DynamicInfo [] []))
         value)
term_strict_getChar x1
  = DI.Term "getChar" (DI.SrcID "Prelude" 0) x1
hook_strict_prim_readFile x1 value
  = DM.eval
      (DM.funcDeclHook "prim_readFile"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_readFile x1
  = DI.Term "prim_readFile" (DI.SrcID "Prelude" 0) x1
hook_strict_prim_writeFile x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_writeFile"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_writeFile x1
  = DI.Term "prim_writeFile" (DI.SrcID "Prelude" 0) x1
hook_strict_prim_appendFile x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_appendFile"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_appendFile x1
  = DI.Term "prim_appendFile" (DI.SrcID "Prelude" 0) x1
hook_strict_catchFail x1 x2 value
  = DM.eval
      (DM.funcDeclHook "catchFail"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_catchFail x1
  = DI.Term "catchFail" (DI.SrcID "Prelude" 0) x1
hook_strict_prim_show x1 value
  = DM.eval
      (DM.funcDeclHook "prim_show"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_show x1
  = DI.Term "prim_show" (DI.SrcID "Prelude" 0) x1
hook_strict_getSearchTree x1 value
  = DM.eval
      (DM.funcDeclHook "getSearchTree"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_getSearchTree x1
  = DI.Term "getSearchTree" (DI.SrcID "Prelude" 0) x1
hook_strict_apply x1 x2 value
  = DM.eval
      (DM.funcDeclHook "apply"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_apply x1 = DI.Term "apply" (DI.SrcID "Prelude" 0) x1
hook_strict_cond x1 x2 value
  = DM.eval
      (DM.funcDeclHook "cond"
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_cond x1 = DI.Term "cond" (DI.SrcID "Prelude" 0) x1
hook_op_EqColonLtEq x1 x2 value
  = DM.eval
      (DM.funcDeclHook "=:<="
         (DI.DebugInfo (DI.SrcID "Prelude" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_op_EqColonLtEq x1 = DI.Term "=:<=" (DI.SrcID "Prelude" 0) x1
term_Cons x1 = DI.Term ":" (DI.SrcID "Prelude" 0) x1
term_Tuple2 x1 = DI.Term "(,)" (DI.SrcID "Prelude" 0) x1
term_Tuple3 x1 = DI.Term "(,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple4 x1 = DI.Term "(,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple5 x1 = DI.Term "(,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple6 x1 = DI.Term "(,,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple7 x1 = DI.Term "(,,,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple8 x1 = DI.Term "(,,,,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple9 x1 = DI.Term "(,,,,,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple10 x1 = DI.Term "(,,,,,,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple11 x1 = DI.Term "(,,,,,,,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple12 x1 = DI.Term "(,,,,,,,,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple13 x1
  = DI.Term "(,,,,,,,,,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple14 x1
  = DI.Term "(,,,,,,,,,,,,,)" (DI.SrcID "Prelude" 0) x1
term_Tuple15 x1
  = DI.Term "(,,,,,,,,,,,,,,)" (DI.SrcID "Prelude" 0) x1
term_O x1 = DI.Term "O" (DI.SrcID "Prelude" 0) x1
term_I x1 = DI.Term "I" (DI.SrcID "Prelude" 0) x1
term_Neg x1 = DI.Term "Neg" (DI.SrcID "Prelude" 0) x1
term_Pos x1 = DI.Term "Pos" (DI.SrcID "Prelude" 0) x1
term_Just x1 = DI.Term "Just" (DI.SrcID "Prelude" 0) x1
term_Left x1 = DI.Term "Left" (DI.SrcID "Prelude" 0) x1
term_Right x1 = DI.Term "Right" (DI.SrcID "Prelude" 0) x1
term_Value x1 = DI.Term "Value" (DI.SrcID "Prelude" 0) x1
term_Choice x1 = DI.Term "Choice" (DI.SrcID "Prelude" 0) x1