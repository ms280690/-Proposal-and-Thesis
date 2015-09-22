{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.IO (module Curry.Module.IO) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



import System.IO
import qualified System.IO as SI
import Control.Concurrent
import qualified Control.Exception as CE

-- somehow using an either type did not get the curry class for prim through.
data IOHandle = One Handle | Two Handle Handle deriving (Show,Eq)
type C_Handle = Prim IOHandle

inputHandle, outputHandle :: IOHandle -> Handle
inputHandle  (One h)   = h
inputHandle  (Two h _) = h
outputHandle (One h)   = h
outputHandle (Two _ h) = h


instance Read IOHandle where
  readsPrec = error "reading Handle"

instance Generate IOHandle where
  genFree    = error "free variable of type IO-Handle"
  maxArity _ = error "free variable of type IO-Handle"


instance ConvertCH C_IOMode SI.IOMode where
  toCurry SI.ReadMode = C_ReadMode
  toCurry SI.WriteMode = C_WriteMode
  toCurry SI.AppendMode = C_AppendMode

  fromCurry C_ReadMode = SI.ReadMode
  fromCurry C_WriteMode = SI.WriteMode
  fromCurry C_AppendMode = SI.AppendMode

instance ConvertCH C_SeekMode SI.SeekMode where
  toCurry SI.AbsoluteSeek = C_AbsoluteSeek
  toCurry SI.RelativeSeek = C_RelativeSeek
  toCurry SI.SeekFromEnd  = C_SeekFromEnd

  fromCurry C_AbsoluteSeek = SI.AbsoluteSeek
  fromCurry C_RelativeSeek = SI.RelativeSeek
  fromCurry C_SeekFromEnd  = SI.SeekFromEnd

stdin :: Result C_Handle
stdin _ = PrimValue (One SI.stdin)

stdout :: Result C_Handle
stdout _ = PrimValue (One SI.stdout)

stderr :: Result C_Handle
stderr _ = PrimValue (One SI.stderr)

prim_openFile :: List C_Char -> C_IOMode -> Result (C_IO C_Handle)
prim_openFile = ioFunc2 (\ s m -> do
  h <- SI.openFile s m
  Prelude.return (One h))

prim_hClose :: C_Handle -> Result (C_IO T0)
prim_hClose = ioFunc1 (\ eh -> case eh of
  One h     -> SI.hClose h
  Two h1 h2 -> SI.hClose h1 Prelude.>> SI.hClose h2)

prim_hFlush :: C_Handle -> Result (C_IO T0)
prim_hFlush = ioFunc1 (SI.hFlush . outputHandle)

prim_hIsEOF :: C_Handle -> Result (C_IO C_Bool)
prim_hIsEOF = ioFunc1 (SI.hIsEOF . inputHandle)

prim_hSeek :: C_Handle -> C_SeekMode -> C_Int -> Result (C_IO T0)
prim_hSeek = ioFunc3 (\ h -> SI.hSeek (inputHandle h))

prim_hWaitForInput ::  C_Handle -> C_Int -> Result (C_IO C_Bool)
prim_hWaitForInput = ioFunc2 (\ h -> myhWaitForInput (inputHandle h))

myhWaitForInput :: SI.Handle -> Int -> IO Bool
myhWaitForInput h i =
  if i Prelude.< 0 
  then SI.hIsEOF h Prelude.>>= Prelude.return . Prelude.not
  else SI.hWaitForInput h i 

selectHandle :: [IOHandle] -> Int -> IO Int
selectHandle handles t = do
  mvar <- newEmptyMVar
  threads <- mapM (\ (i,h) -> forkIO (waitOnHandle (inputHandle h) i t mvar)) 
                  (zip [0..] handles)
  inspectRes (length handles) mvar threads

inspectRes :: Int -> MVar (Maybe Int) -> [ThreadId] ->  IO Int
inspectRes 0 _    _       = Prelude.return (-1)
inspectRes n mvar threads = do
  res <- readMVar mvar
  case res of 
    Nothing -> inspectRes (n-1) mvar threads
    Just v  -> mapM_ killThread threads Prelude.>> Prelude.return v

waitOnHandle :: SI.Handle -> Int -> Int -> MVar (Maybe Int) -> IO ()
waitOnHandle h v t mvar = do
   	    ready <- myhWaitForInput h t
  	    putMVar mvar (if ready then Just v else Nothing)

prim_hWaitForInputs :: List C_Handle -> C_Int -> Result (C_IO C_Int)
prim_hWaitForInputs  = ioFunc2 selectHandle

prim_hGetChar :: C_Handle -> Result (C_IO C_Char)
prim_hGetChar = ioFunc1 (SI.hGetChar . inputHandle)

prim_hPutChar :: C_Handle -> C_Char -> Result (C_IO T0)
prim_hPutChar = ioFunc2 (SI.hPutChar . outputHandle)

prim_hIsReadable :: C_Handle -> Result (C_IO C_Bool)
prim_hIsReadable = ioFunc1 (SI.hIsReadable . inputHandle)

prim_hIsWritable :: C_Handle -> Result (C_IO C_Bool)
prim_hIsWritable = ioFunc1 (SI.hIsWritable . outputHandle)


-- end included

data C_IOMode = C_ReadMode
  | C_WriteMode
  | C_AppendMode
  | C_IOModeFail Curry.RunTimeSystem.C_Exceptions
  | C_IOModeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.IO.C_IOMode)

data C_SeekMode = C_AbsoluteSeek
  | C_RelativeSeek
  | C_SeekFromEnd
  | C_SeekModeFail Curry.RunTimeSystem.C_Exceptions
  | C_SeekModeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.IO.C_SeekMode)

instance BaseCurry Curry.Module.IO.C_IOMode where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.IO.C_IOModeOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.IO.C_ReadMode,Curry.Module.IO.C_WriteMode,Curry.Module.IO.C_AppendMode]))(0)

  failed  = Curry.Module.IO.C_IOModeFail

  branching  = Curry.Module.IO.C_IOModeOr

  consKind (Curry.Module.IO.C_IOModeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.IO.C_IOModeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.IO.C_IOModeFail x) = x

  orRef (Curry.Module.IO.C_IOModeOr x _) = x

  branches (Curry.Module.IO.C_IOModeOr _ x) = x





instance BaseCurry Curry.Module.IO.C_SeekMode where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.IO.C_SeekModeOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.IO.C_AbsoluteSeek,Curry.Module.IO.C_RelativeSeek,Curry.Module.IO.C_SeekFromEnd]))(0)

  failed  = Curry.Module.IO.C_SeekModeFail

  branching  = Curry.Module.IO.C_SeekModeOr

  consKind (Curry.Module.IO.C_SeekModeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.IO.C_SeekModeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.IO.C_SeekModeFail x) = x

  orRef (Curry.Module.IO.C_SeekModeOr x _) = x

  branches (Curry.Module.IO.C_SeekModeOr _ x) = x





instance Curry Curry.Module.IO.C_IOMode where
  strEq Curry.Module.IO.C_ReadMode Curry.Module.IO.C_ReadMode st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.IO.C_WriteMode Curry.Module.IO.C_WriteMode st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.IO.C_AppendMode Curry.Module.IO.C_AppendMode st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.IO.C_ReadMode Curry.Module.IO.C_ReadMode st = Curry.Module.Prelude.C_True
  eq Curry.Module.IO.C_WriteMode Curry.Module.IO.C_WriteMode st = Curry.Module.Prelude.C_True
  eq Curry.Module.IO.C_AppendMode Curry.Module.IO.C_AppendMode st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.IO.C_ReadMode st = Curry.Module.IO.C_ReadMode
  propagate f Curry.Module.IO.C_WriteMode st = Curry.Module.IO.C_WriteMode
  propagate f Curry.Module.IO.C_AppendMode st = Curry.Module.IO.C_AppendMode

  foldCurry f c Curry.Module.IO.C_ReadMode st = c
  foldCurry f c Curry.Module.IO.C_WriteMode st = c
  foldCurry f c Curry.Module.IO.C_AppendMode st = c

  typeName _ = "IOMode"

  showQ _ Curry.Module.IO.C_ReadMode = Prelude.showString("IO.ReadMode")
  showQ _ Curry.Module.IO.C_WriteMode = Prelude.showString("IO.WriteMode")
  showQ _ Curry.Module.IO.C_AppendMode = Prelude.showString("IO.AppendMode")
  showQ _ (Curry.Module.IO.C_IOModeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.IO.C_SeekMode where
  strEq Curry.Module.IO.C_AbsoluteSeek Curry.Module.IO.C_AbsoluteSeek st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.IO.C_RelativeSeek Curry.Module.IO.C_RelativeSeek st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.IO.C_SeekFromEnd Curry.Module.IO.C_SeekFromEnd st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.IO.C_AbsoluteSeek Curry.Module.IO.C_AbsoluteSeek st = Curry.Module.Prelude.C_True
  eq Curry.Module.IO.C_RelativeSeek Curry.Module.IO.C_RelativeSeek st = Curry.Module.Prelude.C_True
  eq Curry.Module.IO.C_SeekFromEnd Curry.Module.IO.C_SeekFromEnd st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.IO.C_AbsoluteSeek st = Curry.Module.IO.C_AbsoluteSeek
  propagate f Curry.Module.IO.C_RelativeSeek st = Curry.Module.IO.C_RelativeSeek
  propagate f Curry.Module.IO.C_SeekFromEnd st = Curry.Module.IO.C_SeekFromEnd

  foldCurry f c Curry.Module.IO.C_AbsoluteSeek st = c
  foldCurry f c Curry.Module.IO.C_RelativeSeek st = c
  foldCurry f c Curry.Module.IO.C_SeekFromEnd st = c

  typeName _ = "SeekMode"

  showQ _ Curry.Module.IO.C_AbsoluteSeek = Prelude.showString("IO.AbsoluteSeek")
  showQ _ Curry.Module.IO.C_RelativeSeek = Prelude.showString("IO.RelativeSeek")
  showQ _ Curry.Module.IO.C_SeekFromEnd = Prelude.showString("IO.SeekFromEnd")
  showQ _ (Curry.Module.IO.C_SeekModeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.IO.C_IOMode where
  showsPrec _ Curry.Module.IO.C_ReadMode = Prelude.showString("ReadMode")
  showsPrec _ Curry.Module.IO.C_WriteMode = Prelude.showString("WriteMode")
  showsPrec _ Curry.Module.IO.C_AppendMode = Prelude.showString("AppendMode")
  showsPrec _ (Curry.Module.IO.C_IOModeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.IO.C_SeekMode where
  showsPrec _ Curry.Module.IO.C_AbsoluteSeek = Prelude.showString("AbsoluteSeek")
  showsPrec _ Curry.Module.IO.C_RelativeSeek = Prelude.showString("RelativeSeek")
  showsPrec _ Curry.Module.IO.C_SeekFromEnd = Prelude.showString("SeekFromEnd")
  showsPrec _ (Curry.Module.IO.C_SeekModeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.IO.C_IOMode where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.IO.C_ReadMode)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("IO")("ReadMode")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.IO.C_WriteMode)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("IO")("WriteMode")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.IO.C_AppendMode)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("IO")("AppendMode")(r)])(r)))





instance Read Curry.Module.IO.C_SeekMode where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.IO.C_AbsoluteSeek)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("IO")("AbsoluteSeek")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.IO.C_RelativeSeek)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("IO")("RelativeSeek")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.IO.C_SeekFromEnd)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("IO")("SeekFromEnd")(r)])(r)))





c_openFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.IO.C_IOMode -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.IO.C_Handle
c_openFile x1 x2 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.IO.c_prim_openFile))(x1)(st))(x2)(st)



c_hClose :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_hClose x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.IO.c_prim_hClose))(x1)(st)



c_hFlush :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_hFlush x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.IO.c_prim_hFlush))(x1)(st)



c_hIsEOF :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_hIsEOF x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.IO.c_prim_hIsEOF))(x1)(st)



c_isEOF :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_isEOF st = Curry.Module.IO.c_hIsEOF(Curry.Module.IO.c_stdin(st))(st)



c_hSeek :: Curry.Module.IO.C_Handle -> Curry.Module.IO.C_SeekMode -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_hSeek x1 x2 x3 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.IO.c_prim_hSeek))(x1)(st))(x2)(st))(x3)(st)



c_hWaitForInput :: Curry.Module.IO.C_Handle -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_hWaitForInput x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.IO.c_prim_hWaitForInput))(x1)(st))(x2)(st)



c_hWaitForInputs :: (Curry.Module.Prelude.List Curry.Module.IO.C_Handle) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_hWaitForInputs x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.IO.c_prim_hWaitForInputs))(x1)(st))(x2)(st)



c_hReady :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_hReady x1 st = Curry.Module.IO.c_hWaitForInput(x1)(Curry.Module.Prelude.C_Zero)(st)



c_hGetChar :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Char
c_hGetChar x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.IO.c_prim_hGetChar))(x1)(st)



c_hGetLine :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_hGetLine x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IO.c_hGetChar(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.IO.c_hGetLine'46_'35lambda2(x1)))(st)



c_hGetLine'46_'35lambda2 :: Curry.Module.IO.C_Handle -> Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_hGetLine'46_'35lambda2 x1 x2 st = Curry.Module.IO.c_hGetLine'46_'35lambda2_case_0(x1)(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\n'))(st))(st)



c_hGetLine'46_'35lambda2'46_'35lambda3 :: Curry.Module.Prelude.C_Char -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_hGetLine'46_'35lambda2'46_'35lambda3 x1 x2 st = Curry.Module.Prelude.c_return((Curry.Module.Prelude.:<)(x1)(x2))(st)



c_hGetContents :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_hGetContents x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IO.c_hIsEOF(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.IO.c_hGetContents'46_'35lambda4(x1)))(st)



c_hGetContents'46_'35lambda4 :: Curry.Module.IO.C_Handle -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_hGetContents'46_'35lambda4 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62(Curry.Module.IO.c_hClose(x1)(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st))(st)
c_hGetContents'46_'35lambda4 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IO.c_hGetChar(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.IO.c_hGetContents'46_'35lambda4'46_'35lambda5(x1)))(st)
c_hGetContents'46_'35lambda4 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.IO.c_hGetContents'46_'35lambda4(x1)(x)(st))(i)(xs)(st)
c_hGetContents'46_'35lambda4 x1 x st = Curry.RunTimeSystem.patternFail("IO.hGetContents._#lambda4")(x)



c_hGetContents'46_'35lambda4'46_'35lambda5 :: Curry.Module.IO.C_Handle -> Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_hGetContents'46_'35lambda4'46_'35lambda5 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IO.c_hGetContents(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.IO.c_hGetContents'46_'35lambda4'46_'35lambda5'46_'35lambda6(x2)))(st)



c_hGetContents'46_'35lambda4'46_'35lambda5'46_'35lambda6 :: Curry.Module.Prelude.C_Char -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_hGetContents'46_'35lambda4'46_'35lambda5'46_'35lambda6 x1 x2 st = Curry.Module.Prelude.c_return((Curry.Module.Prelude.:<)(x1)(x2))(st)



c_getContents :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getContents st = Curry.Module.IO.c_hGetContents(Curry.Module.IO.c_stdin(st))(st)



c_hPutChar :: Curry.Module.IO.C_Handle -> Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_hPutChar x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.IO.c_prim_hPutChar))(x1)(st))(x2)(st)



c_hPutStr :: Curry.Module.IO.C_Handle -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_hPutStr x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_done(st)
c_hPutStr x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.op_62_62(Curry.Module.IO.c_hPutChar(x1)(x3)(st))(Curry.Module.IO.c_hPutStr(x1)(x4)(st))(st)
c_hPutStr x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.IO.c_hPutStr(x1)(x)(st))(i)(xs)(st)
c_hPutStr x1 x st = Curry.RunTimeSystem.patternFail("IO.hPutStr")(x)



c_hPutStrLn :: Curry.Module.IO.C_Handle -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_hPutStrLn x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.IO.c_hPutStr(x1)(x2)(st))(Curry.Module.IO.c_hPutChar(x1)(Curry.Module.Prelude.C_Char('\n'))(st))(st)



c_hPrint :: (Curry t0) => Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0)
c_hPrint x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.IO.c_hPutStrLn(x1)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_show))(st)



c_hIsReadable :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_hIsReadable x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.IO.c_prim_hIsReadable))(x1)(st)



c_hIsWritable :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_hIsWritable x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.IO.c_prim_hIsWritable))(x1)(st)



c_hGetLine'46_'35lambda2_case_0 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st)
c_hGetLine'46_'35lambda2_case_0 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IO.c_hGetLine(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.IO.c_hGetLine'46_'35lambda2'46_'35lambda3(x2)))(st)
c_hGetLine'46_'35lambda2_case_0 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.IO.c_hGetLine'46_'35lambda2_case_0(x1)(x2)(x)(st))(i)(xs)(st)
c_hGetLine'46_'35lambda2_case_0 x1 x2 x st = Curry.RunTimeSystem.patternFail("IO.hGetLine._#lambda2_case_0")(x)



c_stdin :: Curry.RunTimeSystem.State -> Curry.Module.IO.C_Handle
c_stdin st = Curry.Module.IO.stdin(st)



c_stdout :: Curry.RunTimeSystem.State -> Curry.Module.IO.C_Handle
c_stdout st = Curry.Module.IO.stdout(st)



c_stderr :: Curry.RunTimeSystem.State -> Curry.Module.IO.C_Handle
c_stderr st = Curry.Module.IO.stderr(st)



c_prim_openFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.IO.C_IOMode -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.IO.C_Handle
c_prim_openFile x1 x2 st = Curry.Module.IO.prim_openFile(x1)(x2)(st)



c_prim_hClose :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_hClose x1 st = Curry.Module.IO.prim_hClose(x1)(st)



c_prim_hFlush :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_hFlush x1 st = Curry.Module.IO.prim_hFlush(x1)(st)



c_prim_hIsEOF :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_prim_hIsEOF x1 st = Curry.Module.IO.prim_hIsEOF(x1)(st)



c_prim_hSeek :: Curry.Module.IO.C_Handle -> Curry.Module.IO.C_SeekMode -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_hSeek x1 x2 x3 st = Curry.Module.IO.prim_hSeek(x1)(x2)(x3)(st)



c_prim_hWaitForInput :: Curry.Module.IO.C_Handle -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_prim_hWaitForInput x1 x2 st = Curry.Module.IO.prim_hWaitForInput(x1)(x2)(st)



c_prim_hWaitForInputs :: (Curry.Module.Prelude.List Curry.Module.IO.C_Handle) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_prim_hWaitForInputs x1 x2 st = Curry.Module.IO.prim_hWaitForInputs(x1)(x2)(st)



c_prim_hGetChar :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Char
c_prim_hGetChar x1 st = Curry.Module.IO.prim_hGetChar(x1)(st)



c_prim_hPutChar :: Curry.Module.IO.C_Handle -> Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_hPutChar x1 x2 st = Curry.Module.IO.prim_hPutChar(x1)(x2)(st)



c_prim_hIsReadable :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_prim_hIsReadable x1 st = Curry.Module.IO.prim_hIsReadable(x1)(st)



c_prim_hIsWritable :: Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_prim_hIsWritable x1 st = Curry.Module.IO.prim_hIsWritable(x1)(st)


