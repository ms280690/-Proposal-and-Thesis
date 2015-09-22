% DebugMonad.lhs
% Andreas Baldeau, Christoph Wulf, Bernd Brassel
% April 13, 2009

Overview
========

> {-# LANGUAGE FlexibleContexts #-}

> module Curry.Debugger.DebugMonad (
>    module Curry.Debugger.DebugInfo, 
>    module Curry.Debugger.DebugMonad,
>    module Curry.Debugger.Logic) where

Imports
=======

> import Control.Monad.State

> import Curry.Debugger.Oracle
> import Curry.Debugger.Logic
> import Curry.Debugger.DebugInfo
> import Data.Generics
> import Data.Maybe (fromJust)
> import qualified Data.Map as Map

Definitions
===========

`Hook`
------

> data Hook = FuncCallHook String -- function name
>           | FuncDeclHook String -- function name
>           | ConsHook ConsHookType
>           | CaseHook
>           | BranchHook
>           | FreeHook
>           | OrHook
>           | LetHook
>  deriving Show

`ConsHookType`
--------------

> data ConsHookType = Constructor      
>                   | Literal
>                   | PartCall
>  deriving Show

`ExceptionHook`
---------------

> data ExceptionHook = ErrorHook String -- Prelude.error
>                    | FailedHook       -- Prelude.failed
>                    | NEPCaseHook DI   -- non exhaustive patterns at case
>                    | NEPRulesHook DI  -- non exhaustive patterns at functions
>  deriving Show

`Func`
------

Type for representations of functions/constructors.
Contains a Term with function name and static information of the declaration
and the function itself.

> data (DM dm) => Func dm a b = FuncFail
>                             | FuncOr OrRef [Func dm a b]
>                             | FuncUnderscore
>                             | FuncRep Term (a -> dm b)

`GenTerm` instance for function representation: extracts the contained term.

> instance DM dm => GenTerm (Func dm a b) where

TODO: deprecated
	>   underscore = FuncUnderscore

>   genTerm (FuncRep term _) = term
>   genTerm x = genericTerm undefined x

> instance Data (Func dm a b) 
> instance Typeable (Func dm a b) 

Underscore
----------

Returns the representation of a not evaluated term for the result type, 3rd constructor.

> underscore :: GenTerm a => a
> underscore = let res = fromConstr $ indexConstr (dataTypeOf res) 3 in res

Logic
-----

Returns the representation of a failed calculation for the result type, 1st constructor.

> failed :: GenTerm a => a
> failed = let res = fromConstr $ indexConstr (dataTypeOf res) 1 in res

> (?) :: (GenTerm a,DM dm) => a -> a -> dm a
> x ? y = do 
>   ref <- newOrRef
>   return (mkOr ref [x,y])

> mkOr :: GenTerm a => OrRef -> [a] -> a
> mkOr ref xs = 
>   let orWithUndefined  = fromConstr $ indexConstr (dataTypeOf (head xs)) 2
>       orWithArgs = gmapT (castArgs ref xs) orWithUndefined
>    in orWithArgs

apply given function to all branches

> inline :: (DM m,GenTerm a, GenTerm a2) => 
>    ConstraintStore -> OrRef -> (a -> m a2) -> a -> m a2
> inline cs r f x = do
>   args <- mapM choose (zip [0..1] $ branches x)
>   return (mkOr r args)
>  where
>    choose (i,x') = do
>      setConstraintStore (insertBind cs r i)
>      res <- f x'
>      setConstraintStore cs
>      return res


the or branches of a given term

> branches :: (Data a) => a -> [a]
> branches x = x !@ 1

> castArgs :: (Data a,Data b) => OrRef -> [a] -> b -> b
> castArgs ref xs _ = case cast ref of
>   Just r' -> r'
>   Nothing -> case cast xs of
>     Just xs' -> xs'
>     Nothing  -> error "castArgs: not applied to argument of Or"

(!@) retrieves the ith argument of a given constructor

> (!@) :: (Data a,Typeable b) => a -> Int -> b
> x !@ i = fromJust (gmapQi i cast x)

the or reference of a given term

> orRef :: Data a => a -> OrRef
> orRef x = x !@ 0

the function called from generated code to treat remaining
cases

> treatCase :: (DM dm,GenTerm a,GenTerm b) => dm b -> (a -> dm b) -> a -> dm b
> treatCase failhook f x 
>  | constrIndex (toConstr x) <= 2 = treatCase' True f x
>  | otherwise                     = failhook

> treatCase' count f x =
>   let meval = if count then eval else id in
>     case constrIndex (toConstr x) of
>      1 -> return failed
>      2 -> do
>         let r = orRef x
>         cs <- getConstraintStore
>         case lookupRef cs r of 
>           Nothing -> meval $ inline cs r (treatCase' count f) x
>           Just i  -> treatCase' count f (branches x !! i)
>      _ -> f x

> term s =  Term s (SrcID "Prelude" 0)

Creates a term or a given static info and a constructor added by the transformation.

> genericTerm :: GenTerm a => StaticInfo -> a -> Term
> genericTerm si x = case constrIndex (toConstr x) of
>    1 -> TermFail si 
>    2 -> TermOr si (orRef x) (map genTerm (branches x))
>    3 -> TermUnderscore si
>    _ -> error "genericTerm: unexpected argument"

> peekType :: TypeRep -> [Int] -> TypeRep
> peekType tyRep indexes = peekType' (typeRepArgs tyRep) indexes
>   where
>     peekType' :: [TypeRep] -> [Int] -> TypeRep
>     peekType' tyReps [i]    = tyReps !! i
>     peekType' tyReps (i:is) = peekType (tyReps !! i) is


Classes
=======

`OracleState`
-------------

> class OracleState om where
>     getOracle :: om Oracle
>     setOracle :: Oracle -> om ()
>     getExt :: om [String]
>     setExt :: [String] -> om ()
>     newOrRef :: om OrRef
>     getConstraintStore :: om ConstraintStore
>     setConstraintStore :: ConstraintStore -> om ()


`DM`
----

> class (Monad dm,OracleState dm) => DM dm where
>     hook :: GenTerm a => Hook -> DI -> dm a -> dm a
>     hook _ _ ma = ma

>     funcCallHook :: GenTerm a => String -> DI -> dm a -> dm a
>     funcCallHook name = hook (FuncCallHook name)

>     funcDeclHook :: GenTerm a => String -> DI -> dm a -> dm a
>     funcDeclHook name = hook (FuncDeclHook name)

>     consHook :: GenTerm a => ConsHookType -> DI -> dm a -> dm a
>     consHook cht = hook (ConsHook cht)

>     constructorHook :: GenTerm a => DI -> dm a -> dm a
>     constructorHook = consHook Constructor

>     litHook :: GenTerm a => DI -> dm a -> dm a
>     litHook = consHook Literal

>     partCallHook :: GenTerm a => DI -> dm a -> dm a
>     partCallHook = consHook PartCall

>     caseHook :: GenTerm a => DI -> dm a -> dm a
>     caseHook = hook CaseHook 

>     branchHook :: GenTerm a => DI -> dm a -> dm a
>     branchHook = hook BranchHook 

>     freeHook :: GenTerm a => DI -> dm a -> dm a
>     freeHook = hook FreeHook 

>     orHook :: GenTerm a => DI -> dm a -> dm a
>     orHook = hook OrHook 

>     letHook :: GenTerm a => DI -> dm a -> dm a
>     letHook = hook LetHook 

>     exceptionHook :: GenTerm a => ExceptionHook -> dm a
>     exceptionHook _ = return failed

>     errorHook :: GenTerm a => String -> dm a
>     errorHook s = exceptionHook (ErrorHook s)

>     failedHook :: GenTerm a => dm a
>     failedHook = exceptionHook FailedHook

>     nepCaseHook :: GenTerm a => DI -> dm a
>     nepCaseHook di = exceptionHook (NEPCaseHook di)

>     nepRulesHook :: GenTerm a => DI -> dm a
>     nepRulesHook di = exceptionHook (NEPRulesHook di)

Functions
=========

Acessing the oracle
-------------------

> popOracle :: DM dm => dm Bool
> popOracle = do
>     o <- getOracle
>     let (o', b) = pop o
>     setOracle o'
>     return b

> pushOracle :: DM dm => Bool -> dm ()
> pushOracle b = do
>     o <- getOracle
>     let o' = push o b
>     setOracle o'

`eval`
------

> eval :: (GenTerm a, DM dm) => dm a -> dm a
> eval act = do
>     needed <- popOracle
>     if needed then act else return underscore

Accessing virtual I/O
---------------------

Extracts and returns the next ext value from the oracle.

> getNextExtVal :: (Read a, OracleState om, Monad om) => om a
> getNextExtVal = do
>   vals <- getExt
>   setExt $ tail vals
>   return $ read $ head vals


Monad transformer
=================

> type DebugT st dm = StateT (DebugState st) dm

> data DebugState st = DebugState {
>        oracle :: Oracle,
>        ext :: [String],
>        toolstate :: st,
>        nextOrIdx :: Integer,
>        constraints :: ConstraintStore
>      }
>  deriving Show

> runDebugT :: Monad dm => DebugT st dm a -> dm a
> runDebugT ma = 
>  (runStateT ma $ DebugState { oracle    = undefined, 
>                               ext       = undefined, 
>                               toolstate = undefined,
>                               nextOrIdx = 1,
>                               constraints = emptyConstraintStore })
>              >>= return . fst

> newtype DMT st m a = DMT { runDMT :: DebugT st m a }

> instance Monad m => Monad (DMT st m) where
>   a >>= b  = DMT (runDMT a >>= runDMT . b)
>   return x = DMT (return x)

> instance Monad m => OracleState (DMT st m) where
>   getOracle   = DMT (get >>= return . oracle)
>   setOracle o = DMT (get >>= \ dst -> put dst{oracle=o})
>   getExt      = DMT (get >>= return . ext)
>   setExt ext  = DMT (get >>= \ dst -> put dst{ext=ext})
>   newOrRef    = DMT (do 
>                   dst <- get
>                   let idx = nextOrIdx dst
>                       idx' = idx+1
>                   seq idx' (put dst{nextOrIdx=idx'})
>                   return idx)
>   getConstraintStore   = DMT (get >>= return . constraints)
>   setConstraintStore s = DMT (get >>= \ dst -> put dst{constraints=s})


> getToolState :: Monad dm => DMT st dm st
> getToolState = DMT (get >>= return . toolstate)

> putToolState :: Monad dm => st -> DMT st dm ()
> putToolState st = DMT (get >>= \ dst -> put dst{toolstate=st})

> modifyToolState :: Monad dm => (st -> st) -> DMT st dm ()
> modifyToolState f = DMT (get >>= \ dst ->
>                                    put dst{toolstate=(f $ toolstate dst)})

> instance MonadTrans (DMT st) where
>     lift m = DMT (lift m)

> instance MonadIO m => MonadIO (DMT st m) where
>    liftIO = lift . liftIO

> debug :: (DM dm,MonadIO dm) => dm a -> String -> dm a 
> debug oa sfile = do
>     ora <- liftIO $ readStepsFile sfile
>     setOracle ora
>     ext <- liftIO $ readExtFile sfile
>     setExt ext
>     oa

> debugDMT :: (DM (DMT st dm),MonadIO dm) => 
>             DMT st dm a -> String -> st -> dm a
> debugDMT exp steps st = 
>   runDebugT (runDMT (debug (maybePrintOracle (putToolState st >> exp)) steps))

> maybePrintOracle action = do
>            res <- action
>            o <- getOracle
>            unless (o==emptyOracle) 
>                   (liftIO $ putStrLn "oracle not empty!" >> print o)
>            return res



