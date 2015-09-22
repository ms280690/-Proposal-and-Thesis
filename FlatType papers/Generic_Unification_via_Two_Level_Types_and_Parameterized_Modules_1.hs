
module Generic_Unification_via_Two_Level_Types_and_Parameterized_Modules_1 where

import Data.STRef
import Control.Monad.ST
import Control.Monad

type Ptr a = STRef a (Maybe (TypeExp a))

data TypeExp a = 	MutVar (Ptr a)
				| 	GenVar Int
				| 	OperType String [ TypeExp a ]
prune :: TypeExp a -> ST a (TypeExp a)
prune t = case t of
	MutVar r -> do 
		{ m <- readSTRef r ; case m of
			Nothing -> return t
			Just t2 -> do { tx <- prune t2 ; writeSTRef r (Just tx) ; return tx}
		}
	_ 		-> return t

occursInType :: Ptr a -> TypeExp a -> ST a Bool
occursInType r t = do { tx <- prune t ; case tx of
	MutVar r2 -> return(r==r2)
	GenVar _ -> return False
	OperType _ ts -> do { bs <- mapM (occursInType r) ts ; return(or bs)}}

unifyType :: TypeExp a -> TypeExp a -> ST a ()
unifyType t1 t2 = do { t1x <- prune t1; t2x <- prune t2 ; case (t1x,t2x) of
	(MutVar r1, MutVar r2) 				-> unless (r1 == r2) $ writeSTRef r1 (Just t2x)
	(MutVar r1, _) 		   				-> do { b <- occursInType r1 t2x; if b then error "occurs in" else writeSTRef r1 (Just t2x) }
	(_,MutVar _) 		   				-> unifyType t2x t1x
	(GenVar n,GenVar m)    		  		-> unless (n == m) $ error "different genvars"
	(OperType n1 ts1,OperType n2 ts2) 	-> if n1==n2 then unifyArgs ts1 ts2 else error "different constructors"
	(_,_) 								-> error "different types"
} where 
	unifyArgs (x:xs) (y:ys) = do { unifyType x y; unifyArgs xs ys }
	unifyArgs [] [] = return ()
	unifyArgs _ _ = error "different lengths"
	
instantiate :: [TypeExp a] -> TypeExp a -> TypeExp a
instantiate ts x = case x of
	MutVar _ -> x
	OperType nm xs -> OperType nm (map (instantiate ts) xs)
	GenVar n -> ts !! n