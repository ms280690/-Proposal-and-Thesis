import FlatCurryGoodies
import FlatCurry

main = readFlatCurry "Prelude" >>= writeFile "P.fcy" . show . addWorld

addWorld :: Prog -> Prog
addWorld = 
  updProgTypes (map (updTypeConsDecls (map (updConsArgs (map addWorldTE))))) .
  updProgTypes (map (updTypeSynonym addWorldTE)) .
  updProgFuncs addWorldFuncs

addWorldTE :: TypeExpr -> TypeExpr
addWorldTE = trTypeExpr TVar addWorldToIO FuncType
  where
    addWorldToIO mn args 
      | isIO mn   = FuncType unit (TCons mn args)
      | otherwise = TCons mn args

prelude = "Prelude"

isIO :: QName -> Bool
isIO (m,n) = n=="IO" && m==prelude

unit :: TypeExpr
unit = TCons (prelude,"()") []

addWorldFuncs :: [FuncDecl] -> [FuncDecl]
addWorldFuncs = map (updFuncType addWorldTE) .
                map changeExtIOFunc 


changeExtIOFunc :: FuncDecl -> FuncDecl
changeExtIOFunc func
  | isExternal func && isTCons typ && isIO (tConsName typ) 
  = updFuncArity (+1) func
  | otherwise = func
  where
    typ   = resultType (funcType func) 
    arity = funcArity func