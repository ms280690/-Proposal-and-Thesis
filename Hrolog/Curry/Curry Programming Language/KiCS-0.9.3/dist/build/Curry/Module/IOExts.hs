{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.IOExts (module Curry.Module.IOExts) where

import Curry.RunTimeSystem
import Curry.Module.IO
import Curry.Module.Prelude
import Curry.Module.System



-- begin included



import Data.IORef
import qualified Data.IORef as Ref
import System.Process
import Network
import qualified Network.Socket as SO
import System.IO.Unsafe
import Control.Concurrent
import System.IO

import Curry.Module.IO
import qualified Curry.Module.Global

instance Eq (List C_Char) where
  List == List = True
  List == (_ :< _) = False
  (_ :< _) == List = False
  (C_Char c :< xs) == (C_Char c' :< ys) = c Prelude.== c' && xs Prelude.== ys

type Assocs = [(C_String,C_String)]

assocs :: Ref.IORef Assocs
assocs = unsafePerformIO (Ref.newIORef [])

getAssocs :: IO Assocs
getAssocs = Ref.readIORef assocs

setAssocs :: Assocs -> IO ()
setAssocs as = Ref.writeIORef assocs as

prim_execCmd :: List C_Char -> Result (C_IO (T3 C_Handle C_Handle C_Handle))
prim_execCmd = ioFunc1 (\s -> do
     (h1,h2,h3,_) <- runInteractiveCommand s
     Prelude.return (One h1,One h2,One h3))

prim_connectToCmd :: List C_Char -> Result (C_IO C_Handle)
prim_connectToCmd = ioFunc1 (\s -> do
  (hin,hout,herr,_) <- runInteractiveCommand s
  forkIO (forwardError herr)
  Prelude.return (Two hout hin))

forwardError :: Handle -> IO ()
forwardError h = do
   eof <- hIsEOF h 
   if eof then Prelude.return ()
          else do
            line <- hGetLine h 
            hPutStrLn System.IO.stderr line
            forwardError h

prim_setAssoc :: List C_Char -> List C_Char -> Result (C_IO T0)
prim_setAssoc key val = ioFunc0 (do 
                    as <- getAssocs 
                    setAssocs ((key,val):as))


prim_getAssoc :: List C_Char -> Result (C_IO (C_Maybe (List C_Char)))
prim_getAssoc key _ = C_IO (\_ -> do 
         as <- getAssocs
         Prelude.return (IOVal (maybe C_Nothing C_Just (lookup key as))))


type C_IORef a = Curry.Module.Global.C_IORef a

newIORef :: Curry t0 => t0 -> Result (C_IO (C_IORef t0))
newIORef = Curry.Module.Global.newIORef

prim_readIORef :: Curry t0 => C_IORef t0 -> Result (C_IO t0)
prim_readIORef = Curry.Module.Global.prim_readIORef

prim_writeIORef :: Curry t0 => C_IORef t0 -> t0 -> Result (C_IO T0)
prim_writeIORef = Curry.Module.Global.prim_writeIORef


-- end included

c_execCmd :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 Curry.Module.IO.C_Handle Curry.Module.IO.C_Handle Curry.Module.IO.C_Handle)
c_execCmd x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_prim_execCmd))(x1)(st)



c_connectToCommand :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.IO.C_Handle
c_connectToCommand x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_prim_connectToCmd))(x1)(st)



c_readCompleteFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_readCompleteFile x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_readCompleteFile'46_'35lambda2))(st)



c_readCompleteFile'46f'467 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List t0) -> t1 -> Curry.RunTimeSystem.State -> t1
c_readCompleteFile'46f'467 x1@Curry.Module.Prelude.List x2 st = x2
c_readCompleteFile'46f'467 x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.IOExts.c_readCompleteFile'46f'467(x4)(x2)(st)
c_readCompleteFile'46f'467 (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.IOExts.c_readCompleteFile'46f'467(x)(x2)(st))(i)(xs)(st)
c_readCompleteFile'46f'467 x x2 st = Curry.RunTimeSystem.patternFail("IOExts.readCompleteFile.f.7")(x)



c_readCompleteFile'46_'35lambda2 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_readCompleteFile'46_'35lambda2 x1 st = Curry.Module.IOExts.c_readCompleteFile'46f'467(x1)(Curry.Module.Prelude.c_return(x1)(st))(st)



c_updateFile :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_updateFile x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readCompleteFile(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_updateFile'46_'35lambda3(x1)(x2)))(st)



c_updateFile'46_'35lambda3 :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_updateFile'46_'35lambda3 x1 x2 x3 st = Curry.Module.Prelude.c_writeFile(x2)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)



c_exclusiveIO :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_IO t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_exclusiveIO x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.System.c_system(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(x1)(st))(st))(Curry.Module.Prelude.c_catchFail(Curry.Module.Prelude.op_62_62_61(x2)(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_exclusiveIO'46_'35lambda4(x1)))(st))(Curry.Module.Prelude.op_62_62(Curry.Module.System.c_system(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st))(st)



c_exclusiveIO'46_'35lambda4 :: (Curry t44) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t44 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t44
c_exclusiveIO'46_'35lambda4 x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.System.c_system(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(x1)(st))(st))(Curry.Module.Prelude.c_return(x2)(st))(st)



c_setAssoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_setAssoc x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.IOExts.c_prim_setAssoc))(x1)(st))(x2)(st)



c_getAssoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getAssoc x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_prim_getAssoc))(x1)(st)



c_readIORef :: (Curry t0) => (Curry.Module.IOExts.C_IORef t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_readIORef x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_prim_readIORef))(x1)(st)



c_writeIORef :: (Curry t0) => (Curry.Module.IOExts.C_IORef t0) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeIORef x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.IOExts.c_prim_writeIORef))(x1)(st))(x2)(st)



c_prim_execCmd :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 Curry.Module.IO.C_Handle Curry.Module.IO.C_Handle Curry.Module.IO.C_Handle)
c_prim_execCmd x1 st = Curry.Module.IOExts.prim_execCmd(x1)(st)



c_prim_connectToCmd :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.IO.C_Handle
c_prim_connectToCmd x1 st = Curry.Module.IOExts.prim_connectToCmd(x1)(st)



c_prim_setAssoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_setAssoc x1 x2 st = Curry.Module.IOExts.prim_setAssoc(x1)(x2)(st)



c_prim_getAssoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_prim_getAssoc x1 st = Curry.Module.IOExts.prim_getAssoc(x1)(st)



c_newIORef :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.IOExts.C_IORef t0)
c_newIORef x1 st = Curry.Module.IOExts.newIORef(x1)(st)



c_prim_readIORef :: (Curry t0) => (Curry.Module.IOExts.C_IORef t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_prim_readIORef x1 st = Curry.Module.IOExts.prim_readIORef(x1)(st)



c_prim_writeIORef :: (Curry t0) => (Curry.Module.IOExts.C_IORef t0) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_writeIORef x1 x2 st = Curry.Module.IOExts.prim_writeIORef(x1)(x2)(st)


