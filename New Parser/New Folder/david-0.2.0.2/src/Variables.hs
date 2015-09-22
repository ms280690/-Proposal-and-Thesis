module Variables (
   VSet
  , vDelete 
  , vDiff
  , vDiffCommon
  , vEmpty
  , vInsert
  , vIntersect
  , vIntersectSymDiff   
  , vUnion
  ) where

import Syntax

type VSet = [VariableName]

vEmpty :: VSet
vEmpty = []

vGenericTwo :: (Bool,Bool,Bool) -> VSet -> VSet -> (VSet,VSet)
vGenericTwo (fLT,fEQ,fGT) [] [] = ([],[])
vGenericTwo (fLT,fEQ,fGT) xs [] = if fLT then (xs,[]) else ([],xs)
vGenericTwo (fLT,fEQ,fGT) [] ys = if fGT then ([],ys) else (ys,[])
vGenericTwo (fLT,fEQ,fGT) (x:xs) (y:ys) = let
  first  f (a,b) = (f a,b)
  second f (a,b) = (a,f b)
  choice q = if q then first else second
  (which,elt,r1,r2) = case x `compare` y of
    LT    -> (choice fLT , x, xs, y:ys)
    EQ    -> (choice fEQ , x, xs,   ys)
    GT    -> (choice fGT , y, x:xs, ys)
  in which (elt:) $ vGenericTwo (fLT,fEQ,fGT) r1 r2

vDiffCommon, vIntersectSymDiff :: VSet -> VSet -> (VSet,VSet)
vIntersectSymDiff = vGenericTwo (False,True,False)
vDiffCommon = vGenericTwo (True,False,False)

vDiff, vUnion, vIntersect :: VSet -> VSet -> VSet
vDiff      = (fst .) . vDiffCommon
vUnion     = (fst .) . vGenericTwo (True,True,True)
vIntersect = (fst .) . vIntersectSymDiff


vInsert,vDelete :: VariableName -> VSet -> VSet
vInsert x = vUnion [x]
vDelete x = (`vDiff` [x])

