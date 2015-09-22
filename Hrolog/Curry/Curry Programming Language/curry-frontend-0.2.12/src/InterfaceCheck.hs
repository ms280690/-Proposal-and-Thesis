-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
-- InterfaceCheck - Checks the equality of the interfaces of two FlatCurry 
--                  programs 
--
-- January 2006,
-- Martin Engelke (men@informatik.uni-kiel.de)
--
module InterfaceCheck where

import Data.List

import Curry.ExtendedFlat.Type



-------------------------------------------------------------------------------

-- Checks whether the interfaces of two FlatCurry programs are equal 
interfaceCheck :: Prog -> Prog -> Bool
interfaceCheck (Prog m1 is1 ts1 fs1 os1) (Prog m2 is2 ts2 fs2 os2)
   = m1 == m2 
     && sort is1 == sort is2
     && checkTypeDecls ts1 ts2
     && checkFuncDecls fs1 fs2
     && checkOpDecls os1 os2


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

--
checkTypeDecls :: [TypeDecl] -> [TypeDecl] -> Bool
checkTypeDecls ts1 [] = null ts1
checkTypeDecls ts1 ((Type qname vis2 is2 cs2):ts2')
   = let (mt,ts1') = extract (isDataType qname) ts1
     in  maybe False 
               (\ (Type _ vis1 is1 cs1) 
		-> vis1 == vis2 
		   && is1 == is2 
		   && checkConsDecls cs1 cs2
		   && checkTypeDecls ts1' ts2')
	       mt
checkTypeDecls ts1 ((TypeSyn qname vis2 is2 texpr2):ts2')
   = let (mt,ts1') = extract (isTypeSyn qname) ts1
     in  maybe False
	       (\ (TypeSyn _ vis1 is1 texpr1)
		-> vis1 == vis2
		   && is1 == is2
		   && texpr1 == texpr2
		   && checkTypeDecls ts1' ts2')
	       mt

--
checkConsDecls :: [ConsDecl] -> [ConsDecl] -> Bool
checkConsDecls cs1 [] = null cs1
checkConsDecls cs1 ((Cons qname arity2 vis2 texprs2):cs2')
   = let (mc,cs1') = extract (isCons qname) cs1
     in  maybe False
	       (\ (Cons _ arity1 vis1 texprs1)
		-> arity1 == arity2
		   && vis1 == vis2
		   && texprs1 == texprs2
		   && checkConsDecls cs1' cs2')
	       mc

--
checkFuncDecls :: [FuncDecl] -> [FuncDecl] -> Bool
checkFuncDecls fs1 [] = null fs1
checkFuncDecls fs1 ((Func qname arity2 vis2 texpr2 rule2):fs2')
   = let (mf,fs1') = extract (isFunc qname) fs1
     in  maybe False
	       (\ (Func _ arity1 vis1 texpr1 rule1)
		-> arity1 == arity2
		   && vis1 == vis2
		   && texpr1 == texpr2
		   && checkRule rule1 rule2
		   && checkFuncDecls fs1' fs2')
	       mf

--
checkRule :: Rule -> Rule -> Bool
checkRule (Rule _ _)   (Rule _ _)   = True
checkRule (External _) (External _) = True
checkRule _            _            = False

--
checkOpDecls :: [OpDecl] -> [OpDecl] -> Bool
checkOpDecls os1 [] = null os1
checkOpDecls os1 ((Op qname fix2 prec2):os2')
   = let (mo,os1') = extract (isOp qname) os1
     in  maybe False
	       (\ (Op _ fix1 prec1)
		-> prec1 == prec2
		   && fix1 == fix2
		   && checkOpDecls os1' os2')
	       mo


-------------------------------------------------------------------------------

--
isDataType :: QName -> TypeDecl -> Bool
isDataType qname (Type qname' _ _ _) = qname == qname'
isDataType _     _                   = False

--
isTypeSyn :: QName -> TypeDecl -> Bool
isTypeSyn qname (TypeSyn qname' _ _ _) = qname == qname'
isTypeSyn _     _                      = False

--
isCons :: QName -> ConsDecl -> Bool
isCons qname (Cons qname' _ _ _) = qname == qname'

--
isFunc :: QName -> FuncDecl -> Bool
isFunc qname (Func qname' _ _ _ _) = qname == qname'

--
isOp :: QName -> OpDecl -> Bool
isOp qname (Op qname' _ _) = qname == qname'


-------------------------------------------------------------------------------

--
extract :: (a -> Bool) -> [a] -> (Maybe a, [a])
extract _ [] = (Nothing, [])
extract c (x:xs) | c x       = (Just x, xs)
		 | otherwise = let (res, xs') = extract c xs in (res, x:xs')

{-
-- Alternativ:
extract :: (a -> Bool) -> [a] -> (Maybe a, [a])
extract c xs = maybe (Nothing, xs) (\x -> (Just x, delete x xs)) (find c xs)
-}


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
