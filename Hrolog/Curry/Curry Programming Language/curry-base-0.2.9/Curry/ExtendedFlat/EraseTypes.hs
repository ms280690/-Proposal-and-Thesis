{- |Erases type annotations in an ExtendedFlat module.
    In functions, it preserves annotations that contain free type variables,
    i.e. type variables which do not occur in the function's type signature.

    In the remaining type annotations, free type variables are replaced by the
    unit type ().

    (c) 2009, Holger Siegel.
-}

module Curry.ExtendedFlat.EraseTypes (eraseTypes) where

import Curry.ExtendedFlat.Type
import Curry.ExtendedFlat.Goodies

-- TODO the use of lists is not very efficient,
-- but since the number of type variables is relatively
-- small, we stick with that for now.
type TVarSet = [TVarIndex]

eraseTypes :: Prog -> Prog
eraseTypes = updProg id id id (map eraseTypesInFunc) id

eraseTypesInFunc :: FuncDecl -> FuncDecl
eraseTypesInFunc (Func qname arity visty funtype rule)
    = Func qname arity visty funtype rule'
    where rule' = eraseTypesInRule (allTVars funtype) rule

eraseTypesInRule :: TVarSet -> Rule -> Rule
eraseTypesInRule _ r@(External _) = r
eraseTypesInRule sigtvars (Rule vars expr) = Rule
  (map (eraseTypesInVar sigtvars) vars) (eraseTypesInExpr sigtvars expr)

eraseTypesInExpr :: TVarSet -> Expr -> Expr
eraseTypesInExpr sigtvars = rnmAllVars (eraseTypesInVar sigtvars)
                          . updQNames (eraseTypesInQName sigtvars)

eraseTypesInVar :: TVarSet -> VarIndex -> VarIndex
eraseTypesInVar sigtvars v = v {typeofVar = vt' } where
  vt = typeofVar v
  usedtvars = maybe [] allTVars vt
  vt' | all (`elem` sigtvars) usedtvars
          = Nothing
      | otherwise
          = fmap (replaceFreeTypesWithEmptyTuple sigtvars) vt

eraseTypesInQName :: TVarSet -> QName -> QName
eraseTypesInQName sigtvars v = v {typeofQName = qt' } where
  qt = typeofQName v
  usedtvars = maybe [] allTVars qt
  qt' | all (`elem` sigtvars) usedtvars
          = Nothing
      | otherwise
          = fmap (replaceFreeTypesWithEmptyTuple sigtvars) qt

allTVars :: TypeExpr -> [TVarIndex]
allTVars t = go t [] where
  go (TVar v)       is = v : is
  go (FuncType x e) is = go x (go e is)
  go (TCons _ ts)   is = foldr go is ts

replaceFreeTypesWithEmptyTuple :: TVarSet -> TypeExpr -> TypeExpr
replaceFreeTypesWithEmptyTuple usedtvars = updTVars foo where
  foo tidx | tidx `elem` usedtvars = TVar tidx
           | otherwise             = TCons (mkQName ("Prelude", "()")) []
