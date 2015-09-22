--- Provides wrapper functions for the program transformation.
---
--- @version Feb, 2007
---
module Wrapper where

import FlatCurry
import FileGoodies (separatorChar,splitDirectoryBaseName)

prelude = "Prelude"
eventMod= "CEventOracle"
oracle  = "Oracle"
ioexts  = "IOExts"

newModName s = let (d,f) = splitDirectoryBaseName s in
   (case d of "." -> ""; _ -> d++[separatorChar])++oracle++f
newModNameQ (m,n) = (newModName m,n)

tRef = TCons (eventMod,"Ref") []
tR = FuncType tRef


lET bs expr = if null bs then expr else Let bs expr

ref :: VarIndex
ref = 1

flatFunc = Comb FuncCall
flatCons = Comb ConsCall

func name = flatFunc (eventMod,name) 

list = foldr colon nil

char = Lit . Charc

string = list . map char

nil = flatCons (prelude,"[]") []
colon x xs = flatCons (prelude,":") [x,xs]

run mod x = func "run" [string mod, x]

event []    r rs x = unfold r rs x
event (_:_) r _  x = collapse r x

collapse r x = func "collapse" [Var r, x]

closeRef r x = func "closeRef" [Var r, x]

extIO x = listFunc 3 (oracle,"lambda_world") [x]

unfold r rs x
  = if null rs then replace r x
     else lET (zip rs (repeat fresh)) (expand r rs x)

replace r x = func "replace" [Var r, x]
expand r rs x = func "expand" [Var r, list (map Var rs), x]

fresh = func "fresh" [unit]
unit = Comb ConsCall (prelude,"()") []

apply f x 
  = case f of
      Comb (FuncPartCall 1) name args
        -> Comb FuncCall name (args++[x])
      Comb (FuncPartCall n) name args
        -> Comb (FuncPartCall (n-1)) name (args++[x])
      Comb (ConsPartCall 1) name args
        -> Comb ConsCall name (args++[x])
      Comb (ConsPartCall n) name args
        -> Comb (ConsPartCall (n-1)) name (args++[x])
      _ -> flatFunc (prelude,"apply") [f,x]

listFunc n name args
  | missing == 0 = Comb FuncCall name args
  | otherwise = Comb (FuncPartCall missing) name args
 where
  missing = n - length args


compose   = listFunc 3 (oracle,"compose")
unknown e = listFunc 1 (oracle,"unknown") [e]

oracleTry :: QName -> [Expr] -> Expr
oracleTry name targs = 
  func "oracleTry" (Comb (FuncPartCall 2) name argsf : argst)
  where
    (argsf,argst) = splitAt (length targs - 2) targs

partCons,partFunc :: Int -> Expr -> Expr
partCons = apply . partial pc
partFunc = apply . partial pf

pc = listFunc 3 (oracle,"partCons")
pf = listFunc 3 (oracle,"partFunc")
partCall = listFunc 2 (oracle,"partCall")

partial :: ([Expr] -> Expr) -> Int -> Expr
partial part n
  = foldr1 (\f g -> compose [f,g])
  . map (\ (k,p) -> dotted (k-1) (p [])) 
  $ reverse (zip (reverse [1..n]) (part:repeat partCall))

dotted n p
  | n == 0 = p
  | otherwise = dotted (n-1) (compose [p])

inOraclePartCall name = listFunc (arityInOracle name) (oracle,name)

errorCall x = Comb FuncCall (prelude,"error") [string x]

safeIOResult act = listFunc 1 (oracle,"safeIOResult") [act]

specialIOs = zip (repeat prelude) ["return","catchFail","getSearchTree"]

implementedInOracle = ("Unsafe","prim_unsafePerformIO") : ("Meta","headNormalFormIO") : 
  zip (repeat prelude) ["apply","$!","$!!","$#","$##",">>=","catchFail"]
arityInOracle n = if n=="prim_unsafePerformIO" then 2 else 3

addOrc path outdir s = maybe path id outdir ++ (newModName s)
addFcy = (++".fcy")


