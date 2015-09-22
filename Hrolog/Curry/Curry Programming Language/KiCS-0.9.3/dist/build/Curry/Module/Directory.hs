{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Directory (module Curry.Module.Directory) where

import Curry.RunTimeSystem
import Curry.Module.Prelude
import Curry.Module.Time



-- begin included



import System.Time
import System.Directory
import System.IO

prim_doesFileExist :: C_String -> Result (C_IO C_Bool)
prim_doesFileExist = ioFunc1 doesFileExist

prim_doesDirectoryExist :: C_String -> Result (C_IO C_Bool)
prim_doesDirectoryExist = ioFunc1 doesDirectoryExist

prim_fileSize :: C_String -> Result (C_IO C_Int)
prim_fileSize = ioFunc1 (\s->do h <- openFile s ReadMode 
                                i <- hFileSize h
                                hClose h
                                Prelude.return i)

prim_getModificationTime :: C_String -> Result (C_IO C_ClockTime)
prim_getModificationTime = ioFunc1 getModificationTime

prim_getDirectoryContents :: C_String -> Result (C_IO (List C_String))
prim_getDirectoryContents = ioFunc1 getDirectoryContents

getCurrentDirectory :: Result (C_IO C_String)
getCurrentDirectory = ioFunc0 System.Directory.getCurrentDirectory

prim_createDirectory :: C_String -> Result (C_IO T0)
prim_createDirectory = ioFunc1 createDirectory

prim_removeFile :: C_String -> Result (C_IO T0)
prim_removeFile = ioFunc1 removeFile

prim_setCurrentDirectory :: C_String -> Result (C_IO T0)
prim_setCurrentDirectory = ioFunc1 setCurrentDirectory

prim_removeDirectory :: C_String -> Result (C_IO T0)
prim_removeDirectory = ioFunc1 removeDirectory

prim_renameFile :: C_String -> C_String -> Result (C_IO T0)
prim_renameFile = ioFunc2 renameFile

prim_renameDirectory :: C_String -> C_String -> Result (C_IO T0)
prim_renameDirectory = ioFunc2 renameDirectory

-- end included

c_doesFileExist :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_doesFileExist x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Directory.c_prim_doesFileExist))(x1)(st)



c_doesDirectoryExist :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_doesDirectoryExist x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Directory.c_prim_doesDirectoryExist))(x1)(st)



c_fileSize :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_fileSize x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Directory.c_prim_fileSize))(x1)(st)



c_getModificationTime :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_ClockTime
c_getModificationTime x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Directory.c_prim_getModificationTime))(x1)(st)



c_setCurrentDirectory :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_setCurrentDirectory x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Directory.c_prim_setCurrentDirectory))(x1)(st)



c_getDirectoryContents :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getDirectoryContents x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Directory.c_prim_getDirectoryContents))(x1)(st)



c_createDirectory :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_createDirectory x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Directory.c_prim_createDirectory))(x1)(st)



c_removeFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_removeFile x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Directory.c_prim_removeFile))(x1)(st)



c_removeDirectory :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_removeDirectory x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Directory.c_prim_removeDirectory))(x1)(st)



c_renameFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_renameFile x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Directory.c_prim_renameFile))(x1)(st))(x2)(st)



c_renameDirectory :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_renameDirectory x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Directory.c_prim_renameDirectory))(x1)(st))(x2)(st)



c_prim_doesFileExist :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_prim_doesFileExist x1 st = Curry.Module.Directory.prim_doesFileExist(x1)(st)



c_prim_doesDirectoryExist :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_prim_doesDirectoryExist x1 st = Curry.Module.Directory.prim_doesDirectoryExist(x1)(st)



c_prim_fileSize :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_prim_fileSize x1 st = Curry.Module.Directory.prim_fileSize(x1)(st)



c_prim_getModificationTime :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_ClockTime
c_prim_getModificationTime x1 st = Curry.Module.Directory.prim_getModificationTime(x1)(st)



c_getCurrentDirectory :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getCurrentDirectory st = Curry.Module.Directory.getCurrentDirectory(st)



c_prim_setCurrentDirectory :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_setCurrentDirectory x1 st = Curry.Module.Directory.prim_setCurrentDirectory(x1)(st)



c_prim_getDirectoryContents :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_prim_getDirectoryContents x1 st = Curry.Module.Directory.prim_getDirectoryContents(x1)(st)



c_prim_createDirectory :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_createDirectory x1 st = Curry.Module.Directory.prim_createDirectory(x1)(st)



c_prim_removeFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_removeFile x1 st = Curry.Module.Directory.prim_removeFile(x1)(st)



c_prim_removeDirectory :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_removeDirectory x1 st = Curry.Module.Directory.prim_removeDirectory(x1)(st)



c_prim_renameFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_renameFile x1 x2 st = Curry.Module.Directory.prim_renameFile(x1)(x2)(st)



c_prim_renameDirectory :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_renameDirectory x1 x2 st = Curry.Module.Directory.prim_renameDirectory(x1)(x2)(st)


