{-
  Turn recursive data declarations into recursive
  function calls.

  Only single recursive declarations are transformed.
  Mutually recursive declarations are left unchanged.
  You should use transformation UnMutual first.

  (c) 2009, Holger Siegel.
-}

module Curry.ExtendedFlat.LiftLetrec(liftLetrecProg) where

import Data.List
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

import Curry.ExtendedFlat.Type
import Curry.ExtendedFlat.Goodies
import Curry.ExtendedFlat.MonadicGoodies



data LifterState = LifterState { modname :: String,
                                 currentFunc :: String,
                                 globals :: Set.Set QName,
                                 globalCounter :: Map.Map QName Int,
                                 localCounter :: Int,
                                 lifted :: Map.Map QName FuncDecl }


type Bind = (VarIndex, Expr)    -- (name, value)
type LiftMonad = State LifterState


liftLetrecProg :: Prog -> Prog
liftLetrecProg prog = updProg id id id (++ fdecls) id prog'
    where state = LifterState {
                    modname = progName prog,
                    currentFunc = "anonymous",
                    globals = Set.fromList g,
                    globalCounter = Map.fromList $ zip g (repeat 1),
                    localCounter = 0,
                    lifted = Map.empty
                  }
          g = allGlobals prog
          (prog', state') = runState (updProgFuncsM run prog) state
          fdecls = Map.elems (lifted state')
          run fdecl = do
            let fname = localName (funcName fdecl)
            modify (\st -> st { currentFunc  = fname,
                                localCounter = (maximum . map idxOf . allVarsInFunc) fdecl
                              })
            fdecl' <- updFuncLetsM liftRecursion fdecl
            modify (\st -> st {currentFunc = "anonymous"})
            return fdecl'



liftRecursion :: [Bind] -> Expr -> LiftMonad Expr
liftRecursion [(b, rhs)] body
    | b `elem` fv = do globalcall <- mkLiftedFunction (typeofVar b) b rhs (fv \\ [b])
                       return (Let [(b, globalcall)] body)
    | otherwise  = return (Let [(b, rhs)] body)
    where fv = fvs rhs
liftRecursion bs body = return (Let bs body)


mkLiftedFunction :: Maybe TypeExpr -> VarIndex -> Expr -> [VarIndex] -> LiftMonad Expr
mkLiftedFunction t v rhs fv 
    = do name <- newGlobalName t
         st <- get
         let fcall = (Comb FuncCall name (map Var fv))
         let fdecl = Func name (length fv) Private (fromMaybe (TVar 0) t) (Rule fv (Let [(v,fcall)] rhs))
         put st { lifted = Map.insert name fdecl (lifted st),
                  globals = Set.insert name (globals st)
                }
         return fcall


newGlobalName :: Maybe TypeExpr -> LiftMonad QName
newGlobalName t
    = do st <- get
         let qn = QName Nothing t (modname st) (currentFunc st)
         let counter = Map.findWithDefault 1 qn (globalCounter st)
         put st { globalCounter = Map.insert qn (counter + 1) (globalCounter st) }
         let qn' = QName Nothing t (modname st) (localName qn ++ "_" ++ show counter)
         if qn' `Set.member` globals st
             then newGlobalName t
             else return qn'


allGlobals :: Prog -> [QName]
allGlobals prog = [n | Func n _ _ _ _ <- fs]
    where fs = progFuncs prog
