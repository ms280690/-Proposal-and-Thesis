{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Socket (module Curry.Module.Socket) where

import Curry.RunTimeSystem
import Curry.Module.IO
import Curry.Module.Prelude
import Curry.Module.System



-- begin included



import Network
import Network.Socket 
import Control.Concurrent
import System.IO (Handle)

type C_Socket = Prim Socket

instance Read Socket where

instance Generate Socket where
  genFree  = error "no random sockets"
  maxArity = error "no narrowing on sockets"

instance ConvertCH C_Int PortID where
  toCurry (PortNumber i) = toCurry (toInteger i)
  fromCurry i = PortNumber (fromInteger (fromCurry i))

prim_listenOn :: C_Int -> Result (C_IO C_Socket)
prim_listenOn  = Curry.Module.Prelude.ioFunc1 listenOn 

listenOnFresh :: Result (C_IO (T2 C_Int C_Socket))
listenOnFresh  = Curry.Module.Prelude.ioFunc0 listenOnFreshPort

listenOnFreshPort :: IO (PortID,Socket)
listenOnFreshPort = do
  s <- listenOn (PortNumber aNY_PORT)
  p <- Network.socketPort s
  Prelude.return (p,s)


prim_socketListen :: C_Socket -> C_Int -> Result (C_IO T0)
prim_socketListen  = Curry.Module.Prelude.ioFunc2 listen

prim_socketAccept :: C_Socket -> Result (C_IO (T2 (List C_Char) C_Handle))
prim_socketAccept  = 
  ioFunc1 (\ s -> do {(h,s,_) <- Network.accept s; Prelude.return (s,One h)})

prim_waitForSocketAccept :: C_Socket -> C_Int -> Result (C_IO (C_Maybe (T2 (List C_Char) C_Handle)))
prim_waitForSocketAccept  = Curry.Module.Prelude.ioFunc2 wait

wait :: Socket -> Int -> IO (Maybe (String,IOHandle))
wait s t = do
  mv <- newEmptyMVar
  tacc <- forkIO (do {(h,s,_) <- Network.accept s; putMVar mv (Just (s,One h))})
  ttim <- forkIO (do {threadDelay (t*1000); putMVar mv Nothing})
  res <- takeMVar mv
  maybe (killThread tacc) (\_ -> killThread ttim) res
  Prelude.return res

prim_connectToSocket :: List C_Char -> C_Int -> Result (C_IO C_Handle)
prim_connectToSocket  = 
  ioFunc2 (\ s i -> do {ct <- connectTo s i; Prelude.return (One ct)})



-- end included

c_listenOn :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Socket.C_Socket
c_listenOn x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Socket.c_prim_listenOn))(x1)(st)



c_socketListen :: Curry.Module.Socket.C_Socket -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_socketListen x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Socket.c_prim_socketListen))(x1)(st))(x2)(st)



c_socketAccept :: Curry.Module.Socket.C_Socket -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.IO.C_Handle)
c_socketAccept x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Socket.c_prim_socketAccept))(x1)(st)



c_waitForSocketAccept :: Curry.Module.Socket.C_Socket -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.IO.C_Handle))
c_waitForSocketAccept x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Socket.c_prim_waitForSocketAccept))(x1)(st))(x2)(st)



c_connectToSocket :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.IO.C_Handle
c_connectToSocket x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Socket.c_prim_connectToSocket))(x1)(st))(x2)(st)



c_prim_listenOn :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Socket.C_Socket
c_prim_listenOn x1 st = Curry.Module.Socket.prim_listenOn(x1)(st)



c_listenOnFresh :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Socket.C_Socket)
c_listenOnFresh st = Curry.Module.Socket.listenOnFresh(st)



c_prim_socketListen :: Curry.Module.Socket.C_Socket -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_socketListen x1 x2 st = Curry.Module.Socket.prim_socketListen(x1)(x2)(st)



c_prim_socketAccept :: Curry.Module.Socket.C_Socket -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.IO.C_Handle)
c_prim_socketAccept x1 st = Curry.Module.Socket.prim_socketAccept(x1)(st)



c_prim_waitForSocketAccept :: Curry.Module.Socket.C_Socket -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.IO.C_Handle))
c_prim_waitForSocketAccept x1 x2 st = Curry.Module.Socket.prim_waitForSocketAccept(x1)(x2)(st)



c_prim_connectToSocket :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.IO.C_Handle
c_prim_connectToSocket x1 x2 st = Curry.Module.Socket.prim_connectToSocket(x1)(x2)(st)


