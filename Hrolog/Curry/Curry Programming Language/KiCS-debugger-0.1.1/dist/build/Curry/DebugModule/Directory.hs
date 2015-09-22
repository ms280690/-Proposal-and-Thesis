{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Directory where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Prelude
import qualified Curry.DebugModule.Time
 
strict_prim_doesFileExist ::
                          (DM.DM dm) =>
                            Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                              dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Bool)
strict_prim_doesFileExist x0
  = hook_strict_prim_doesFileExist x0
      (Prelude.error "not implemented")
 
strict_prim_doesDirectoryExist ::
                               (DM.DM dm) =>
                                 Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                   dm
                                     (Curry.DebugModule.Prelude.IO dm
                                        Curry.DebugModule.Prelude.Bool)
strict_prim_doesDirectoryExist x0
  = hook_strict_prim_doesDirectoryExist x0
      (Prelude.error "not implemented")
 
strict_prim_fileSize ::
                     (DM.DM dm) =>
                       Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                         dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Int)
strict_prim_fileSize x0
  = hook_strict_prim_fileSize x0 (Prelude.error "not implemented")
 
strict_prim_getModificationTime ::
                                (DM.DM dm) =>
                                  Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                    dm
                                      (Curry.DebugModule.Prelude.IO dm
                                         Curry.DebugModule.Time.ClockTime)
strict_prim_getModificationTime x0
  = hook_strict_prim_getModificationTime x0
      (Prelude.error "not implemented")
 
strict_getCurrentDirectory ::
                           (DM.DM dm) =>
                             dm
                               (Curry.DebugModule.Prelude.IO dm
                                  (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_getCurrentDirectory
  = hook_strict_getCurrentDirectory (Prelude.error "not implemented")
 
strict_prim_setCurrentDirectory ::
                                (DM.DM dm) =>
                                  Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                    dm
                                      (Curry.DebugModule.Prelude.IO dm
                                         Curry.DebugModule.Prelude.Unit)
strict_prim_setCurrentDirectory x0
  = hook_strict_prim_setCurrentDirectory x0
      (Prelude.error "not implemented")
 
strict_prim_getDirectoryContents ::
                                 (DM.DM dm) =>
                                   Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                     dm
                                       (Curry.DebugModule.Prelude.IO dm
                                          (Curry.DebugModule.Prelude.List
                                             (Curry.DebugModule.Prelude.List
                                                Curry.DebugModule.Prelude.Char)))
strict_prim_getDirectoryContents x0
  = hook_strict_prim_getDirectoryContents x0
      (Prelude.error "not implemented")
 
strict_prim_createDirectory ::
                            (DM.DM dm) =>
                              Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_prim_createDirectory x0
  = hook_strict_prim_createDirectory x0
      (Prelude.error "not implemented")
 
strict_prim_removeFile ::
                       (DM.DM dm) =>
                         Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                           dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_prim_removeFile x0
  = hook_strict_prim_removeFile x0 (Prelude.error "not implemented")
 
strict_prim_removeDirectory ::
                            (DM.DM dm) =>
                              Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_prim_removeDirectory x0
  = hook_strict_prim_removeDirectory x0
      (Prelude.error "not implemented")
 
strict_prim_renameFile ::
                       (DM.DM dm) =>
                         Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                           Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                             dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_prim_renameFile x0 x1
  = hook_strict_prim_renameFile x0 x1
      (Prelude.error "not implemented")
 
strict_prim_renameDirectory ::
                            (DM.DM dm) =>
                              Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                  dm
                                    (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_prim_renameDirectory x0 x1
  = hook_strict_prim_renameDirectory x0 x1
      (Prelude.error "not implemented")
 
strict_doesFileExist ::
                     (DM.DM dm) =>
                       Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                         dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Bool)
strict_doesFileExist x1
  = DM.eval
      (DM.funcDeclHook "doesFileExist"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_doesFileExist [])
                        strict_prim_doesFileExist)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_doesFileExist x1
  = DI.Term "doesFileExist" (DI.SrcID "Directory" 0) x1
 
strict_doesDirectoryExist ::
                          (DM.DM dm) =>
                            Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                              dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Bool)
strict_doesDirectoryExist x1
  = DM.eval
      (DM.funcDeclHook "doesDirectoryExist"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_doesDirectoryExist [])
                        strict_prim_doesDirectoryExist)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_doesDirectoryExist x1
  = DI.Term "doesDirectoryExist" (DI.SrcID "Directory" 0) x1
 
strict_fileSize ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                    dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Int)
strict_fileSize x1
  = DM.eval
      (DM.funcDeclHook "fileSize"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_fileSize []) strict_prim_fileSize)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_fileSize x1
  = DI.Term "fileSize" (DI.SrcID "Directory" 0) x1
 
strict_getModificationTime ::
                           (DM.DM dm) =>
                             Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                               dm
                                 (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Time.ClockTime)
strict_getModificationTime x1
  = DM.eval
      (DM.funcDeclHook "getModificationTime"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_getModificationTime [])
                        strict_prim_getModificationTime)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_getModificationTime x1
  = DI.Term "getModificationTime" (DI.SrcID "Directory" 0) x1
 
strict_setCurrentDirectory ::
                           (DM.DM dm) =>
                             Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                               dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_setCurrentDirectory x1
  = DM.eval
      (DM.funcDeclHook "setCurrentDirectory"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_setCurrentDirectory [])
                        strict_prim_setCurrentDirectory)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_setCurrentDirectory x1
  = DI.Term "setCurrentDirectory" (DI.SrcID "Directory" 0) x1
 
strict_getDirectoryContents ::
                            (DM.DM dm) =>
                              Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                dm
                                  (Curry.DebugModule.Prelude.IO dm
                                     (Curry.DebugModule.Prelude.List
                                        (Curry.DebugModule.Prelude.List
                                           Curry.DebugModule.Prelude.Char)))
strict_getDirectoryContents x1
  = DM.eval
      (DM.funcDeclHook "getDirectoryContents"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_getDirectoryContents [])
                        strict_prim_getDirectoryContents)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_getDirectoryContents x1
  = DI.Term "getDirectoryContents" (DI.SrcID "Directory" 0) x1
 
strict_createDirectory ::
                       (DM.DM dm) =>
                         Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                           dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_createDirectory x1
  = DM.eval
      (DM.funcDeclHook "createDirectory"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_createDirectory [])
                        strict_prim_createDirectory)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_createDirectory x1
  = DI.Term "createDirectory" (DI.SrcID "Directory" 0) x1
 
strict_removeFile ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                      dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_removeFile x1
  = DM.eval
      (DM.funcDeclHook "removeFile"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_removeFile [])
                        strict_prim_removeFile)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_removeFile x1
  = DI.Term "removeFile" (DI.SrcID "Directory" 0) x1
 
strict_removeDirectory ::
                       (DM.DM dm) =>
                         Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                           dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_removeDirectory x1
  = DM.eval
      (DM.funcDeclHook "removeDirectory"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_removeDirectory [])
                        strict_prim_removeDirectory)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_removeDirectory x1
  = DI.Term "removeDirectory" (DI.SrcID "Directory" 0) x1
 
strict_renameFile ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                      Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                        dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_renameFile x1 x2
  = DM.eval
      (DM.funcDeclHook "renameFile"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_renameFile [])
                                 strict_prim_renameFile)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$##"
                        (DI.DebugInfo (DI.SrcID "Directory" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhombRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x5 x6)))
term_strict_renameFile x1
  = DI.Term "renameFile" (DI.SrcID "Directory" 0) x1
 
strict_renameDirectory ::
                       (DM.DM dm) =>
                         Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                           Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                             dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_renameDirectory x1 x2
  = DM.eval
      (DM.funcDeclHook "renameDirectory"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_renameDirectory [])
                                 strict_prim_renameDirectory)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$##"
                        (DI.DebugInfo (DI.SrcID "Directory" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhombRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "Directory" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x5 x6)))
term_strict_renameDirectory x1
  = DI.Term "renameDirectory" (DI.SrcID "Directory" 0) x1
hook_strict_prim_doesFileExist x1 value
  = DM.eval
      (DM.funcDeclHook "prim_doesFileExist"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_doesFileExist x1
  = DI.Term "prim_doesFileExist" (DI.SrcID "Directory" 0) x1
hook_strict_prim_doesDirectoryExist x1 value
  = DM.eval
      (DM.funcDeclHook "prim_doesDirectoryExist"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_doesDirectoryExist x1
  = DI.Term "prim_doesDirectoryExist" (DI.SrcID "Directory" 0) x1
hook_strict_prim_fileSize x1 value
  = DM.eval
      (DM.funcDeclHook "prim_fileSize"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_fileSize x1
  = DI.Term "prim_fileSize" (DI.SrcID "Directory" 0) x1
hook_strict_prim_getModificationTime x1 value
  = DM.eval
      (DM.funcDeclHook "prim_getModificationTime"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_getModificationTime x1
  = DI.Term "prim_getModificationTime" (DI.SrcID "Directory" 0) x1
hook_strict_getCurrentDirectory value
  = DM.eval
      (DM.funcDeclHook "getCurrentDirectory"
         (DI.DebugInfo (DI.SrcID "Directory" 0) (DI.DynamicInfo [] []))
         value)
term_strict_getCurrentDirectory x1
  = DI.Term "getCurrentDirectory" (DI.SrcID "Directory" 0) x1
hook_strict_prim_setCurrentDirectory x1 value
  = DM.eval
      (DM.funcDeclHook "prim_setCurrentDirectory"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_setCurrentDirectory x1
  = DI.Term "prim_setCurrentDirectory" (DI.SrcID "Directory" 0) x1
hook_strict_prim_getDirectoryContents x1 value
  = DM.eval
      (DM.funcDeclHook "prim_getDirectoryContents"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_getDirectoryContents x1
  = DI.Term "prim_getDirectoryContents" (DI.SrcID "Directory" 0) x1
hook_strict_prim_createDirectory x1 value
  = DM.eval
      (DM.funcDeclHook "prim_createDirectory"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_createDirectory x1
  = DI.Term "prim_createDirectory" (DI.SrcID "Directory" 0) x1
hook_strict_prim_removeFile x1 value
  = DM.eval
      (DM.funcDeclHook "prim_removeFile"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_removeFile x1
  = DI.Term "prim_removeFile" (DI.SrcID "Directory" 0) x1
hook_strict_prim_removeDirectory x1 value
  = DM.eval
      (DM.funcDeclHook "prim_removeDirectory"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_removeDirectory x1
  = DI.Term "prim_removeDirectory" (DI.SrcID "Directory" 0) x1
hook_strict_prim_renameFile x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_renameFile"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_renameFile x1
  = DI.Term "prim_renameFile" (DI.SrcID "Directory" 0) x1
hook_strict_prim_renameDirectory x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_renameDirectory"
         (DI.DebugInfo (DI.SrcID "Directory" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_renameDirectory x1
  = DI.Term "prim_renameDirectory" (DI.SrcID "Directory" 0) x1