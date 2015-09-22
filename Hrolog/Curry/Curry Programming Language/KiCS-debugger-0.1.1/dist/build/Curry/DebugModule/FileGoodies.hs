{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.FileGoodies where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Directory
import qualified Curry.DebugModule.List
import qualified Curry.DebugModule.Prelude
 
strict_separatorChar ::
                     (DM.DM dm) => dm Curry.DebugModule.Prelude.Char
strict_separatorChar
  = DM.eval
      (DM.funcDeclHook "separatorChar"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
         (DM.litHook
            (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
            (Prelude.return (Curry.DebugModule.Prelude.Char '/'))))
 
strict_pathSeparatorChar ::
                         (DM.DM dm) => dm Curry.DebugModule.Prelude.Char
strict_pathSeparatorChar
  = DM.eval
      (DM.funcDeclHook "pathSeparatorChar"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
         (DM.litHook
            (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
            (Prelude.return (Curry.DebugModule.Prelude.Char ':'))))
 
strict_suffixSeparatorChar ::
                           (DM.DM dm) => dm Curry.DebugModule.Prelude.Char
strict_suffixSeparatorChar
  = DM.eval
      (DM.funcDeclHook "suffixSeparatorChar"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
         (DM.litHook
            (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
            (Prelude.return (Curry.DebugModule.Prelude.Char '.'))))
 
strict_isAbsolute ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                      dm Curry.DebugModule.Prelude.Bool
strict_isAbsolute x1
  = DM.eval
      (DM.funcDeclHook "isAbsolute"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_19"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_19 x2)))
term_strict_isAbsolute x1
  = DI.Term "isAbsolute" (DI.SrcID "FileGoodies" 0) x1
 
strict_dirName ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                   dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_dirName x1
  = DM.eval
      (DM.funcDeclHook "dirName"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "splitDirectoryBaseName"
                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_splitDirectoryBaseName x2)
             DM.funcCallHook "fst"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (Curry.DebugModule.Prelude.strict_fst x3)))
term_strict_dirName x1
  = DI.Term "dirName" (DI.SrcID "FileGoodies" 0) x1
 
strict_baseName ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                    dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_baseName x1
  = DM.eval
      (DM.funcDeclHook "baseName"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "splitDirectoryBaseName"
                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_splitDirectoryBaseName x2)
             DM.funcCallHook "snd"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (Curry.DebugModule.Prelude.strict_snd x3)))
term_strict_baseName x1
  = DI.Term "baseName" (DI.SrcID "FileGoodies" 0) x1
 
strict_splitDirectoryBaseName ::
                              (DM.DM dm) =>
                                Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                  dm
                                    (Curry.DebugModule.Prelude.Tuple2
                                       (Curry.DebugModule.Prelude.List
                                          Curry.DebugModule.Prelude.Char)
                                       (Curry.DebugModule.Prelude.List
                                          Curry.DebugModule.Prelude.Char))
strict_splitDirectoryBaseName x1
  = DM.eval
      (DM.funcDeclHook "splitDirectoryBaseName"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (DM.letHook
            (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
            (do x2 <- do x10 <- do x7 <- do x5 <- Prelude.return
                                                    (PC.partCall2
                                                       (Curry.DebugModule.Prelude.term_op_EqEq [])
                                                       Curry.DebugModule.Prelude.op_EqEq)
                                            x6 <- DM.funcCallHook "separatorChar"
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo [] []))
                                                    strict_separatorChar
                                            Prelude.return
                                              (PC.partCall1
                                                 (Curry.DebugModule.Prelude.term_strict_flip
                                                    [DI.genTerm x5, DI.genTerm x6])
                                                 (Curry.DebugModule.Prelude.strict_flip x5 x6))
                                   DM.funcCallHook "break"
                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                        (DI.DynamicInfo [] [DI.genTerm x7]))
                                     (Curry.DebugModule.Prelude.strict_break x7)
                         x11 <- do x8 <- DM.funcCallHook "reverse"
                                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                              (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_reverse
                                   x9 <- Prelude.return x1
                                   DM.funcCallHook "apply"
                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                        (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                     (Curry.DebugModule.Prelude.strict_apply x8 x9)
                         DM.funcCallHook "apply"
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                              (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                           (Curry.DebugModule.Prelude.strict_apply x10 x11)
                DM.eval
                  (DM.letHook
                     (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                     (do x3 <- do x12 <- Prelude.return x2
                                  DM.funcCallHook "splitDirectoryBaseName._#selFP3#rbase"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12]))
                                    (x'xstrict_splitDirectoryBaseName46_35selFP335rbase x12)
                         DM.eval
                           (DM.letHook
                              (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                              (do x4 <- do x13 <- Prelude.return x2
                                           DM.funcCallHook "splitDirectoryBaseName._#selFP4#rdir"
                                             (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x13]))
                                             (x'xstrict_splitDirectoryBaseName46_35selFP435rdir x13)
                                  DM.eval
                                    (do x15 <- Prelude.return x3
                                        x16 <- Prelude.return x4
                                        x17 <- do x14 <- Prelude.return x4
                                                  DM.funcCallHook "null"
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo [] [DI.genTerm x14]))
                                                    (Curry.DebugModule.Prelude.strict_null x14)
                                        DM.funcCallHook "_case_18"
                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                             (DI.DynamicInfo []
                                                [DI.genTerm x15, DI.genTerm x16, DI.genTerm x17]))
                                          (strict__case_18 x15 x16 x17)))))))))
term_strict_splitDirectoryBaseName x1
  = DI.Term "splitDirectoryBaseName" (DI.SrcID "FileGoodies" 0) x1
 
x'xstrict_splitDirectoryBaseName46_35selFP335rbase ::
                                                   (DM.DM dm) =>
                                                     Curry.DebugModule.Prelude.Tuple2
                                                       (Curry.DebugModule.Prelude.List
                                                          Curry.DebugModule.Prelude.Char)
                                                       (Curry.DebugModule.Prelude.List
                                                          Curry.DebugModule.Prelude.Char)
                                                       ->
                                                       dm
                                                         (Curry.DebugModule.Prelude.List
                                                            Curry.DebugModule.Prelude.Char)
x'xstrict_splitDirectoryBaseName46_35selFP335rbase x1
  = DM.eval
      (DM.funcDeclHook "splitDirectoryBaseName._#selFP3#rbase"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_17"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_17 x2)))
x'xterm_strict_splitDirectoryBaseName46_35selFP335rbase x1
  = DI.Term "splitDirectoryBaseName._#selFP3#rbase"
      (DI.SrcID "FileGoodies" 0)
      x1
 
x'xstrict_splitDirectoryBaseName46_35selFP435rdir ::
                                                  (DM.DM dm) =>
                                                    Curry.DebugModule.Prelude.Tuple2
                                                      (Curry.DebugModule.Prelude.List
                                                         Curry.DebugModule.Prelude.Char)
                                                      (Curry.DebugModule.Prelude.List
                                                         Curry.DebugModule.Prelude.Char)
                                                      ->
                                                      dm
                                                        (Curry.DebugModule.Prelude.List
                                                           Curry.DebugModule.Prelude.Char)
x'xstrict_splitDirectoryBaseName46_35selFP435rdir x1
  = DM.eval
      (DM.funcDeclHook "splitDirectoryBaseName._#selFP4#rdir"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_16"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_16 x2)))
x'xterm_strict_splitDirectoryBaseName46_35selFP435rdir x1
  = DI.Term "splitDirectoryBaseName._#selFP4#rdir"
      (DI.SrcID "FileGoodies" 0)
      x1
 
strict_stripSuffix ::
                   (DM.DM dm) =>
                     dm
                       (DM.Func dm
                          (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                          (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_stripSuffix
  = DM.eval
      (DM.funcDeclHook "stripSuffix"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return
                     (PC.partCall1 (Curry.DebugModule.Prelude.term_strict_fst [])
                        Curry.DebugModule.Prelude.strict_fst)
             x1 <- Prelude.return
                     (PC.partCall1 (term_strict_splitBaseName []) strict_splitBaseName)
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
               (Curry.DebugModule.Prelude.op_Point x0 x1)))
 
strict_fileSuffix ::
                  (DM.DM dm) =>
                    dm
                      (DM.Func dm
                         (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                         (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_fileSuffix
  = DM.eval
      (DM.funcDeclHook "fileSuffix"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return
                     (PC.partCall1 (Curry.DebugModule.Prelude.term_strict_snd [])
                        Curry.DebugModule.Prelude.strict_snd)
             x1 <- Prelude.return
                     (PC.partCall1 (term_strict_splitBaseName []) strict_splitBaseName)
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
               (Curry.DebugModule.Prelude.op_Point x0 x1)))
 
strict_splitBaseName ::
                     (DM.DM dm) =>
                       Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                         dm
                           (Curry.DebugModule.Prelude.Tuple2
                              (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                              (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_splitBaseName x1
  = DM.eval
      (DM.funcDeclHook "splitBaseName"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (DM.letHook
            (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
            (do x2 <- do x10 <- do x7 <- do x5 <- Prelude.return
                                                    (PC.partCall2
                                                       (Curry.DebugModule.Prelude.term_op_EqEq [])
                                                       Curry.DebugModule.Prelude.op_EqEq)
                                            x6 <- DM.funcCallHook "suffixSeparatorChar"
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo [] []))
                                                    strict_suffixSeparatorChar
                                            Prelude.return
                                              (PC.partCall1
                                                 (Curry.DebugModule.Prelude.term_strict_flip
                                                    [DI.genTerm x5, DI.genTerm x6])
                                                 (Curry.DebugModule.Prelude.strict_flip x5 x6))
                                   DM.funcCallHook "break"
                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                        (DI.DynamicInfo [] [DI.genTerm x7]))
                                     (Curry.DebugModule.Prelude.strict_break x7)
                         x11 <- do x8 <- DM.funcCallHook "reverse"
                                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                              (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_reverse
                                   x9 <- Prelude.return x1
                                   DM.funcCallHook "apply"
                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                        (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                     (Curry.DebugModule.Prelude.strict_apply x8 x9)
                         DM.funcCallHook "apply"
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                              (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                           (Curry.DebugModule.Prelude.strict_apply x10 x11)
                DM.eval
                  (DM.letHook
                     (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                     (do x3 <- do x12 <- Prelude.return x2
                                  DM.funcCallHook "splitBaseName._#selFP6#rsuffix"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12]))
                                    (x'xstrict_splitBaseName46_35selFP635rsuffix x12)
                         DM.eval
                           (DM.letHook
                              (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                              (do x4 <- do x13 <- Prelude.return x2
                                           DM.funcCallHook "splitBaseName._#selFP7#rbase"
                                             (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x13]))
                                             (x'xstrict_splitBaseName46_35selFP735rbase x13)
                                  DM.eval
                                    (do x20 <- Prelude.return x1
                                        x21 <- Prelude.return x3
                                        x22 <- Prelude.return x4
                                        x23 <- do x18 <- do x14 <- Prelude.return x4
                                                            DM.funcCallHook "null"
                                                              (DI.DebugInfo
                                                                 (DI.SrcID "FileGoodies" 0)
                                                                 (DI.DynamicInfo []
                                                                    [DI.genTerm x14]))
                                                              (Curry.DebugModule.Prelude.strict_null
                                                                 x14)
                                                  x19 <- do x16 <- do x15 <- DM.funcCallHook
                                                                               "separatorChar"
                                                                               (DI.DebugInfo
                                                                                  (DI.SrcID
                                                                                     "FileGoodies"
                                                                                     0)
                                                                                  (DI.DynamicInfo []
                                                                                     []))
                                                                               strict_separatorChar
                                                                      DM.funcCallHook "elem"
                                                                        (DI.DebugInfo
                                                                           (DI.SrcID "FileGoodies"
                                                                              0)
                                                                           (DI.DynamicInfo []
                                                                              [DI.genTerm x15]))
                                                                        (Curry.DebugModule.Prelude.strict_elem
                                                                           x15)
                                                            x17 <- Prelude.return x3
                                                            DM.funcCallHook "apply"
                                                              (DI.DebugInfo
                                                                 (DI.SrcID "FileGoodies" 0)
                                                                 (DI.DynamicInfo []
                                                                    [DI.genTerm x16,
                                                                     DI.genTerm x17]))
                                                              (Curry.DebugModule.Prelude.strict_apply
                                                                 x16
                                                                 x17)
                                                  DM.funcCallHook "||"
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo []
                                                          [DI.genTerm x18, DI.genTerm x19]))
                                                    (Curry.DebugModule.Prelude.op_OrOr x18 x19)
                                        DM.funcCallHook "_case_15"
                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                             (DI.DynamicInfo []
                                                [DI.genTerm x20, DI.genTerm x21, DI.genTerm x22,
                                                 DI.genTerm x23]))
                                          (strict__case_15 x20 x21 x22 x23)))))))))
term_strict_splitBaseName x1
  = DI.Term "splitBaseName" (DI.SrcID "FileGoodies" 0) x1
 
x'xstrict_splitBaseName46_35selFP635rsuffix ::
                                            (DM.DM dm) =>
                                              Curry.DebugModule.Prelude.Tuple2
                                                (Curry.DebugModule.Prelude.List
                                                   Curry.DebugModule.Prelude.Char)
                                                (Curry.DebugModule.Prelude.List
                                                   Curry.DebugModule.Prelude.Char)
                                                ->
                                                dm
                                                  (Curry.DebugModule.Prelude.List
                                                     Curry.DebugModule.Prelude.Char)
x'xstrict_splitBaseName46_35selFP635rsuffix x1
  = DM.eval
      (DM.funcDeclHook "splitBaseName._#selFP6#rsuffix"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_14"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_14 x2)))
x'xterm_strict_splitBaseName46_35selFP635rsuffix x1
  = DI.Term "splitBaseName._#selFP6#rsuffix"
      (DI.SrcID "FileGoodies" 0)
      x1
 
x'xstrict_splitBaseName46_35selFP735rbase ::
                                          (DM.DM dm) =>
                                            Curry.DebugModule.Prelude.Tuple2
                                              (Curry.DebugModule.Prelude.List
                                                 Curry.DebugModule.Prelude.Char)
                                              (Curry.DebugModule.Prelude.List
                                                 Curry.DebugModule.Prelude.Char)
                                              ->
                                              dm
                                                (Curry.DebugModule.Prelude.List
                                                   Curry.DebugModule.Prelude.Char)
x'xstrict_splitBaseName46_35selFP735rbase x1
  = DM.eval
      (DM.funcDeclHook "splitBaseName._#selFP7#rbase"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_13"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_13 x2)))
x'xterm_strict_splitBaseName46_35selFP735rbase x1
  = DI.Term "splitBaseName._#selFP7#rbase" (DI.SrcID "FileGoodies" 0)
      x1
 
strict_splitPath ::
                 (DM.DM dm) =>
                   Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                     dm
                       (Curry.DebugModule.Prelude.List
                          (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_splitPath x1
  = DM.eval
      (DM.funcDeclHook "splitPath"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_12"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_12 x2)))
term_strict_splitPath x1
  = DI.Term "splitPath" (DI.SrcID "FileGoodies" 0) x1
 
x'xstrict_splitPath46_35selFP935ys ::
                                   (DM.DM dm) =>
                                     Curry.DebugModule.Prelude.Tuple2
                                       (Curry.DebugModule.Prelude.List
                                          Curry.DebugModule.Prelude.Char)
                                       (Curry.DebugModule.Prelude.List
                                          Curry.DebugModule.Prelude.Char)
                                       ->
                                       dm
                                         (Curry.DebugModule.Prelude.List
                                            Curry.DebugModule.Prelude.Char)
x'xstrict_splitPath46_35selFP935ys x1
  = DM.eval
      (DM.funcDeclHook "splitPath._#selFP9#ys"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_10"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_10 x2)))
x'xterm_strict_splitPath46_35selFP935ys x1
  = DI.Term "splitPath._#selFP9#ys" (DI.SrcID "FileGoodies" 0) x1
 
x'xstrict_splitPath46_35selFP1035zs ::
                                    (DM.DM dm) =>
                                      Curry.DebugModule.Prelude.Tuple2
                                        (Curry.DebugModule.Prelude.List
                                           Curry.DebugModule.Prelude.Char)
                                        (Curry.DebugModule.Prelude.List
                                           Curry.DebugModule.Prelude.Char)
                                        ->
                                        dm
                                          (Curry.DebugModule.Prelude.List
                                             Curry.DebugModule.Prelude.Char)
x'xstrict_splitPath46_35selFP1035zs x1
  = DM.eval
      (DM.funcDeclHook "splitPath._#selFP10#zs"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_9"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_9 x2)))
x'xterm_strict_splitPath46_35selFP1035zs x1
  = DI.Term "splitPath._#selFP10#zs" (DI.SrcID "FileGoodies" 0) x1
 
strict_findFileInPath ::
                      (DM.DM dm) =>
                        dm
                          (DM.Func dm
                             (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                             (DM.Func dm
                                (Curry.DebugModule.Prelude.List
                                   (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
                                (DM.Func dm
                                   (Curry.DebugModule.Prelude.List
                                      (Curry.DebugModule.Prelude.List
                                         Curry.DebugModule.Prelude.Char))
                                   (Curry.DebugModule.Prelude.IO dm
                                      (Curry.DebugModule.Prelude.Maybe
                                         (Curry.DebugModule.Prelude.List
                                            Curry.DebugModule.Prelude.Char))))))
strict_findFileInPath
  = DM.eval
      (DM.funcDeclHook "findFileInPath"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
         (Prelude.return
            (PC.partCall3 (term_strict_lookupFileInPath [])
               strict_lookupFileInPath)))
 
strict_lookupFileInPath ::
                        (DM.DM dm) =>
                          Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                            Curry.DebugModule.Prelude.List
                              (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                              ->
                              Curry.DebugModule.Prelude.List
                                (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                                ->
                                dm
                                  (Curry.DebugModule.Prelude.IO dm
                                     (Curry.DebugModule.Prelude.Maybe
                                        (Curry.DebugModule.Prelude.List
                                           Curry.DebugModule.Prelude.Char)))
strict_lookupFileInPath x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "lookupFileInPath"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- Prelude.return x3
             x8 <- do x4 <- Prelude.return x1
                      DM.funcCallHook "isAbsolute"
                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4]))
                        (strict_isAbsolute x4)
             DM.funcCallHook "_case_8"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8]))
               (strict__case_8 x5 x6 x7 x8)))
term_strict_lookupFileInPath x1
  = DI.Term "lookupFileInPath" (DI.SrcID "FileGoodies" 0) x1
 
x'xstrict_lookupFileInPath46lookupFirstFileWithSuffix4636 ::
                                                          (DM.DM dm) =>
                                                            Curry.DebugModule.Prelude.List
                                                              Curry.DebugModule.Prelude.Char
                                                              ->
                                                              Curry.DebugModule.Prelude.List
                                                                (Curry.DebugModule.Prelude.List
                                                                   Curry.DebugModule.Prelude.Char)
                                                                ->
                                                                dm
                                                                  (Curry.DebugModule.Prelude.IO dm
                                                                     (Curry.DebugModule.Prelude.Maybe
                                                                        (Curry.DebugModule.Prelude.List
                                                                           Curry.DebugModule.Prelude.Char)))
x'xstrict_lookupFileInPath46lookupFirstFileWithSuffix4636 x1 x2
  = DM.eval
      (DM.funcDeclHook "lookupFileInPath.lookupFirstFileWithSuffix.36"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_7"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_7 x3 x4)))
x'xterm_strict_lookupFileInPath46lookupFirstFileWithSuffix4636 x1
  = DI.Term "lookupFileInPath.lookupFirstFileWithSuffix.36"
      (DI.SrcID "FileGoodies" 0)
      x1
 
x'xstrict_lookupFileInPath46lookupFirstFileWithSuffix463646_35lambda3 ::
                                                                      (DM.DM dm) =>
                                                                        Curry.DebugModule.Prelude.List
                                                                          Curry.DebugModule.Prelude.Char
                                                                          ->
                                                                          Curry.DebugModule.Prelude.List
                                                                            Curry.DebugModule.Prelude.Char
                                                                            ->
                                                                            Curry.DebugModule.Prelude.List
                                                                              (Curry.DebugModule.Prelude.List
                                                                                 Curry.DebugModule.Prelude.Char)
                                                                              ->
                                                                              Curry.DebugModule.Prelude.Bool
                                                                                ->
                                                                                dm
                                                                                  (Curry.DebugModule.Prelude.IO
                                                                                     dm
                                                                                     (Curry.DebugModule.Prelude.Maybe
                                                                                        (Curry.DebugModule.Prelude.List
                                                                                           Curry.DebugModule.Prelude.Char)))
x'xstrict_lookupFileInPath46lookupFirstFileWithSuffix463646_35lambda3
  x1 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook
         "lookupFileInPath.lookupFirstFileWithSuffix.36._#lambda3"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- Prelude.return x3
             x8 <- Prelude.return x4
             DM.funcCallHook "_case_6"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8]))
               (strict__case_6 x5 x6 x7 x8)))
x'xterm_strict_lookupFileInPath46lookupFirstFileWithSuffix463646_35lambda3
  x1
  = DI.Term "lookupFileInPath.lookupFirstFileWithSuffix.36._#lambda3"
      (DI.SrcID "FileGoodies" 0)
      x1
 
x'xstrict_lookupFileInPath46lookupFirstFile4636 ::
                                                (DM.DM dm) =>
                                                  Curry.DebugModule.Prelude.List
                                                    Curry.DebugModule.Prelude.Char
                                                    ->
                                                    Curry.DebugModule.Prelude.List
                                                      (Curry.DebugModule.Prelude.List
                                                         Curry.DebugModule.Prelude.Char)
                                                      ->
                                                      Curry.DebugModule.Prelude.List
                                                        (Curry.DebugModule.Prelude.List
                                                           Curry.DebugModule.Prelude.Char)
                                                        ->
                                                        dm
                                                          (Curry.DebugModule.Prelude.IO dm
                                                             (Curry.DebugModule.Prelude.Maybe
                                                                (Curry.DebugModule.Prelude.List
                                                                   Curry.DebugModule.Prelude.Char)))
x'xstrict_lookupFileInPath46lookupFirstFile4636 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "lookupFileInPath.lookupFirstFile.36"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x2
             x6 <- Prelude.return x3
             DM.funcCallHook "_case_5"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_5 x4 x5 x6)))
x'xterm_strict_lookupFileInPath46lookupFirstFile4636 x1
  = DI.Term "lookupFileInPath.lookupFirstFile.36"
      (DI.SrcID "FileGoodies" 0)
      x1
 
x'xstrict_lookupFileInPath46lookupFirstFile463646_35lambda2 ::
                                                            (DM.DM dm) =>
                                                              Curry.DebugModule.Prelude.List
                                                                (Curry.DebugModule.Prelude.List
                                                                   Curry.DebugModule.Prelude.Char)
                                                                ->
                                                                Curry.DebugModule.Prelude.List
                                                                  Curry.DebugModule.Prelude.Char
                                                                  ->
                                                                  Curry.DebugModule.Prelude.List
                                                                    (Curry.DebugModule.Prelude.List
                                                                       Curry.DebugModule.Prelude.Char)
                                                                    ->
                                                                    Curry.DebugModule.Prelude.Maybe
                                                                      (Curry.DebugModule.Prelude.List
                                                                         Curry.DebugModule.Prelude.Char)
                                                                      ->
                                                                      dm
                                                                        (Curry.DebugModule.Prelude.IO
                                                                           dm
                                                                           (Curry.DebugModule.Prelude.Maybe
                                                                              (Curry.DebugModule.Prelude.List
                                                                                 Curry.DebugModule.Prelude.Char)))
x'xstrict_lookupFileInPath46lookupFirstFile463646_35lambda2 x1 x2
  x3 x4
  = DM.eval
      (DM.funcDeclHook "lookupFileInPath.lookupFirstFile.36._#lambda2"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x10 <- do x5 <- Prelude.return x2
                       x6 <- Prelude.return x3
                       x7 <- Prelude.return x1
                       DM.funcCallHook "lookupFileInPath.lookupFirstFile.36"
                         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                            (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                         (x'xstrict_lookupFileInPath46lookupFirstFile4636 x5 x6 x7)
             x11 <- do x8 <- Prelude.return
                               (PC.partCall1 (Curry.DebugModule.Prelude.term_strict_return [])
                                  Curry.DebugModule.Prelude.strict_return)
                       x9 <- Prelude.return
                               (PC.partCall1 (Curry.DebugModule.Prelude.term_Just [])
                                  (\ x0 -> Prelude.return (Curry.DebugModule.Prelude.Just x0)))
                       DM.funcCallHook "."
                         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                            (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                         (Curry.DebugModule.Prelude.op_Point x8 x9)
             x12 <- Prelude.return x4
             DM.funcCallHook "maybe"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x10, DI.genTerm x11, DI.genTerm x12]))
               (Curry.DebugModule.Prelude.strict_maybe x10 x11 x12)))
x'xterm_strict_lookupFileInPath46lookupFirstFile463646_35lambda2 x1
  = DI.Term "lookupFileInPath.lookupFirstFile.36._#lambda2"
      (DI.SrcID "FileGoodies" 0)
      x1
 
strict_getFileInPath ::
                     (DM.DM dm) =>
                       Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                         Curry.DebugModule.Prelude.List
                           (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                           ->
                           Curry.DebugModule.Prelude.List
                             (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                             ->
                             dm
                               (Curry.DebugModule.Prelude.IO dm
                                  (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_getFileInPath x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "getFileInPath"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x9 <- do x4 <- Prelude.return x1
                      x5 <- Prelude.return x2
                      x6 <- Prelude.return x3
                      DM.funcCallHook "lookupFileInPath"
                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
                        (strict_lookupFileInPath x4 x5 x6)
             x10 <- do x7 <- Prelude.return x1
                       x8 <- Prelude.return x3
                       Prelude.return
                         (PC.partCall1
                            (x'xterm_strict_getFileInPath46_35lambda4
                               [DI.genTerm x7, DI.genTerm x8])
                            (x'xstrict_getFileInPath46_35lambda4 x7 x8))
             DM.funcCallHook ">>="
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
               (Curry.DebugModule.Prelude.op_GtGtEq x9 x10)))
term_strict_getFileInPath x1
  = DI.Term "getFileInPath" (DI.SrcID "FileGoodies" 0) x1
 
x'xstrict_getFileInPath46_35lambda4 ::
                                    (DM.DM dm) =>
                                      Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char
                                        ->
                                        Curry.DebugModule.Prelude.List
                                          (Curry.DebugModule.Prelude.List
                                             Curry.DebugModule.Prelude.Char)
                                          ->
                                          Curry.DebugModule.Prelude.Maybe
                                            (Curry.DebugModule.Prelude.List
                                               Curry.DebugModule.Prelude.Char)
                                            ->
                                            dm
                                              (Curry.DebugModule.Prelude.IO dm
                                                 (Curry.DebugModule.Prelude.List
                                                    Curry.DebugModule.Prelude.Char))
x'xstrict_getFileInPath46_35lambda4 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "getFileInPath._#lambda4"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x65 <- do x63 <- Prelude.return
                                (PC.partCall1 (Curry.DebugModule.Prelude.term_strict_error [])
                                   Curry.DebugModule.Prelude.strict_error)
                       x64 <- do x61 <- do x12 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return
                                                       (Curry.DebugModule.Prelude.Char 'F'))
                                           x13 <- do x10 <- DM.litHook
                                                              (DI.DebugInfo
                                                                 (DI.SrcID "FileGoodies" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 (Curry.DebugModule.Prelude.Char
                                                                    'i'))
                                                     x11 <- do x8 <- DM.litHook
                                                                       (DI.DebugInfo
                                                                          (DI.SrcID "FileGoodies" 0)
                                                                          (DI.DynamicInfo [] []))
                                                                       (Prelude.return
                                                                          (Curry.DebugModule.Prelude.Char
                                                                             'l'))
                                                               x9 <- do x6 <- DM.litHook
                                                                                (DI.DebugInfo
                                                                                   (DI.SrcID
                                                                                      "FileGoodies"
                                                                                      0)
                                                                                   (DI.DynamicInfo
                                                                                      []
                                                                                      []))
                                                                                (Prelude.return
                                                                                   (Curry.DebugModule.Prelude.Char
                                                                                      'e'))
                                                                        x7 <- do x4 <- DM.litHook
                                                                                         (DI.DebugInfo
                                                                                            (DI.SrcID
                                                                                               "FileGoodies"
                                                                                               0)
                                                                                            (DI.DynamicInfo
                                                                                               []
                                                                                               []))
                                                                                         (Prelude.return
                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                               ' '))
                                                                                 x5 <- DM.constructorHook
                                                                                         (DI.DebugInfo
                                                                                            (DI.SrcID
                                                                                               "FileGoodies"
                                                                                               0)
                                                                                            (DI.DynamicInfo
                                                                                               []
                                                                                               []))
                                                                                         (Prelude.return
                                                                                            Curry.DebugModule.Prelude.Nil)
                                                                                 DM.constructorHook
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "FileGoodies"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         [DI.genTerm
                                                                                            x4,
                                                                                          DI.genTerm
                                                                                            x5]))
                                                                                   (Prelude.return
                                                                                      (Curry.DebugModule.Prelude.Cons
                                                                                         x4
                                                                                         x5))
                                                                        DM.constructorHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "FileGoodies"
                                                                                0)
                                                                             (DI.DynamicInfo []
                                                                                [DI.genTerm x6,
                                                                                 DI.genTerm x7]))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                x6
                                                                                x7))
                                                               DM.constructorHook
                                                                 (DI.DebugInfo
                                                                    (DI.SrcID "FileGoodies" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x8,
                                                                        DI.genTerm x9]))
                                                                 (Prelude.return
                                                                    (Curry.DebugModule.Prelude.Cons
                                                                       x8
                                                                       x9))
                                                     DM.constructorHook
                                                       (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x10, DI.genTerm x11]))
                                                       (Prelude.return
                                                          (Curry.DebugModule.Prelude.Cons x10 x11))
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x12, DI.genTerm x13]))
                                             (Prelude.return
                                                (Curry.DebugModule.Prelude.Cons x12 x13))
                                 x62 <- do x59 <- Prelude.return x1
                                           x60 <- do x57 <- do x50 <- DM.litHook
                                                                        (DI.DebugInfo
                                                                           (DI.SrcID "FileGoodies"
                                                                              0)
                                                                           (DI.DynamicInfo [] []))
                                                                        (Prelude.return
                                                                           (Curry.DebugModule.Prelude.Char
                                                                              ' '))
                                                               x51 <- do x48 <- DM.litHook
                                                                                  (DI.DebugInfo
                                                                                     (DI.SrcID
                                                                                        "FileGoodies"
                                                                                        0)
                                                                                     (DI.DynamicInfo
                                                                                        []
                                                                                        []))
                                                                                  (Prelude.return
                                                                                     (Curry.DebugModule.Prelude.Char
                                                                                        'n'))
                                                                         x49 <- do x46 <- DM.litHook
                                                                                            (DI.DebugInfo
                                                                                               (DI.SrcID
                                                                                                  "FileGoodies"
                                                                                                  0)
                                                                                               (DI.DynamicInfo
                                                                                                  []
                                                                                                  []))
                                                                                            (Prelude.return
                                                                                               (Curry.DebugModule.Prelude.Char
                                                                                                  'o'))
                                                                                   x47 <- do x44 <- DM.litHook
                                                                                                      (DI.DebugInfo
                                                                                                         (DI.SrcID
                                                                                                            "FileGoodies"
                                                                                                            0)
                                                                                                         (DI.DynamicInfo
                                                                                                            []
                                                                                                            []))
                                                                                                      (Prelude.return
                                                                                                         (Curry.DebugModule.Prelude.Char
                                                                                                            't'))
                                                                                             x45 <- do x42 <- DM.litHook
                                                                                                                (DI.DebugInfo
                                                                                                                   (DI.SrcID
                                                                                                                      "FileGoodies"
                                                                                                                      0)
                                                                                                                   (DI.DynamicInfo
                                                                                                                      []
                                                                                                                      []))
                                                                                                                (Prelude.return
                                                                                                                   (Curry.DebugModule.Prelude.Char
                                                                                                                      ' '))
                                                                                                       x43 <- do x40 <- DM.litHook
                                                                                                                          (DI.DebugInfo
                                                                                                                             (DI.SrcID
                                                                                                                                "FileGoodies"
                                                                                                                                0)
                                                                                                                             (DI.DynamicInfo
                                                                                                                                []
                                                                                                                                []))
                                                                                                                          (Prelude.return
                                                                                                                             (Curry.DebugModule.Prelude.Char
                                                                                                                                'f'))
                                                                                                                 x41 <- do x38 <- DM.litHook
                                                                                                                                    (DI.DebugInfo
                                                                                                                                       (DI.SrcID
                                                                                                                                          "FileGoodies"
                                                                                                                                          0)
                                                                                                                                       (DI.DynamicInfo
                                                                                                                                          []
                                                                                                                                          []))
                                                                                                                                    (Prelude.return
                                                                                                                                       (Curry.DebugModule.Prelude.Char
                                                                                                                                          'o'))
                                                                                                                           x39 <- do x36 <- DM.litHook
                                                                                                                                              (DI.DebugInfo
                                                                                                                                                 (DI.SrcID
                                                                                                                                                    "FileGoodies"
                                                                                                                                                    0)
                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                    []
                                                                                                                                                    []))
                                                                                                                                              (Prelude.return
                                                                                                                                                 (Curry.DebugModule.Prelude.Char
                                                                                                                                                    'u'))
                                                                                                                                     x37 <- do x34 <- DM.litHook
                                                                                                                                                        (DI.DebugInfo
                                                                                                                                                           (DI.SrcID
                                                                                                                                                              "FileGoodies"
                                                                                                                                                              0)
                                                                                                                                                           (DI.DynamicInfo
                                                                                                                                                              []
                                                                                                                                                              []))
                                                                                                                                                        (Prelude.return
                                                                                                                                                           (Curry.DebugModule.Prelude.Char
                                                                                                                                                              'n'))
                                                                                                                                               x35 <- do x32 <- DM.litHook
                                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                                     (DI.SrcID
                                                                                                                                                                        "FileGoodies"
                                                                                                                                                                        0)
                                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                                        []
                                                                                                                                                                        []))
                                                                                                                                                                  (Prelude.return
                                                                                                                                                                     (Curry.DebugModule.Prelude.Char
                                                                                                                                                                        'd'))
                                                                                                                                                         x33 <- do x30 <- DM.litHook
                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                  "FileGoodies"
                                                                                                                                                                                  0)
                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                  []
                                                                                                                                                                                  []))
                                                                                                                                                                            (Prelude.return
                                                                                                                                                                               (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                  ' '))
                                                                                                                                                                   x31 <- do x28 <- DM.litHook
                                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                                            "FileGoodies"
                                                                                                                                                                                            0)
                                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                                            []
                                                                                                                                                                                            []))
                                                                                                                                                                                      (Prelude.return
                                                                                                                                                                                         (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                            'i'))
                                                                                                                                                                             x29 <- do x26 <- DM.litHook
                                                                                                                                                                                                (DI.DebugInfo
                                                                                                                                                                                                   (DI.SrcID
                                                                                                                                                                                                      "FileGoodies"
                                                                                                                                                                                                      0)
                                                                                                                                                                                                   (DI.DynamicInfo
                                                                                                                                                                                                      []
                                                                                                                                                                                                      []))
                                                                                                                                                                                                (Prelude.return
                                                                                                                                                                                                   (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                      'n'))
                                                                                                                                                                                       x27 <- do x24 <- DM.litHook
                                                                                                                                                                                                          (DI.DebugInfo
                                                                                                                                                                                                             (DI.SrcID
                                                                                                                                                                                                                "FileGoodies"
                                                                                                                                                                                                                0)
                                                                                                                                                                                                             (DI.DynamicInfo
                                                                                                                                                                                                                []
                                                                                                                                                                                                                []))
                                                                                                                                                                                                          (Prelude.return
                                                                                                                                                                                                             (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                ' '))
                                                                                                                                                                                                 x25 <- do x22 <- DM.litHook
                                                                                                                                                                                                                    (DI.DebugInfo
                                                                                                                                                                                                                       (DI.SrcID
                                                                                                                                                                                                                          "FileGoodies"
                                                                                                                                                                                                                          0)
                                                                                                                                                                                                                       (DI.DynamicInfo
                                                                                                                                                                                                                          []
                                                                                                                                                                                                                          []))
                                                                                                                                                                                                                    (Prelude.return
                                                                                                                                                                                                                       (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                          'p'))
                                                                                                                                                                                                           x23 <- do x20 <- DM.litHook
                                                                                                                                                                                                                              (DI.DebugInfo
                                                                                                                                                                                                                                 (DI.SrcID
                                                                                                                                                                                                                                    "FileGoodies"
                                                                                                                                                                                                                                    0)
                                                                                                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                                                                                                    []
                                                                                                                                                                                                                                    []))
                                                                                                                                                                                                                              (Prelude.return
                                                                                                                                                                                                                                 (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                    'a'))
                                                                                                                                                                                                                     x21 <- do x18 <- DM.litHook
                                                                                                                                                                                                                                        (DI.DebugInfo
                                                                                                                                                                                                                                           (DI.SrcID
                                                                                                                                                                                                                                              "FileGoodies"
                                                                                                                                                                                                                                              0)
                                                                                                                                                                                                                                           (DI.DynamicInfo
                                                                                                                                                                                                                                              []
                                                                                                                                                                                                                                              []))
                                                                                                                                                                                                                                        (Prelude.return
                                                                                                                                                                                                                                           (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                              't'))
                                                                                                                                                                                                                               x19 <- do x16 <- DM.litHook
                                                                                                                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                                                                                                                     (DI.SrcID
                                                                                                                                                                                                                                                        "FileGoodies"
                                                                                                                                                                                                                                                        0)
                                                                                                                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                                                                                                                        []
                                                                                                                                                                                                                                                        []))
                                                                                                                                                                                                                                                  (Prelude.return
                                                                                                                                                                                                                                                     (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                        'h'))
                                                                                                                                                                                                                                         x17 <- do x14 <- DM.litHook
                                                                                                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                                                                                                  "FileGoodies"
                                                                                                                                                                                                                                                                  0)
                                                                                                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                                                                                                  []
                                                                                                                                                                                                                                                                  []))
                                                                                                                                                                                                                                                            (Prelude.return
                                                                                                                                                                                                                                                               (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                  ' '))
                                                                                                                                                                                                                                                   x15 <- DM.constructorHook
                                                                                                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                                                                                                  "FileGoodies"
                                                                                                                                                                                                                                                                  0)
                                                                                                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                                                                                                  []
                                                                                                                                                                                                                                                                  []))
                                                                                                                                                                                                                                                            (Prelude.return
                                                                                                                                                                                                                                                               Curry.DebugModule.Prelude.Nil)
                                                                                                                                                                                                                                                   DM.constructorHook
                                                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                                                           "FileGoodies"
                                                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                                                           []
                                                                                                                                                                                                                                                           [DI.genTerm
                                                                                                                                                                                                                                                              x14,
                                                                                                                                                                                                                                                            DI.genTerm
                                                                                                                                                                                                                                                              x15]))
                                                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                           x14
                                                                                                                                                                                                                                                           x15))
                                                                                                                                                                                                                                         DM.constructorHook
                                                                                                                                                                                                                                           (DI.DebugInfo
                                                                                                                                                                                                                                              (DI.SrcID
                                                                                                                                                                                                                                                 "FileGoodies"
                                                                                                                                                                                                                                                 0)
                                                                                                                                                                                                                                              (DI.DynamicInfo
                                                                                                                                                                                                                                                 []
                                                                                                                                                                                                                                                 [DI.genTerm
                                                                                                                                                                                                                                                    x16,
                                                                                                                                                                                                                                                  DI.genTerm
                                                                                                                                                                                                                                                    x17]))
                                                                                                                                                                                                                                           (Prelude.return
                                                                                                                                                                                                                                              (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                 x16
                                                                                                                                                                                                                                                 x17))
                                                                                                                                                                                                                               DM.constructorHook
                                                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                                                       "FileGoodies"
                                                                                                                                                                                                                                       0)
                                                                                                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                                                                                                       []
                                                                                                                                                                                                                                       [DI.genTerm
                                                                                                                                                                                                                                          x18,
                                                                                                                                                                                                                                        DI.genTerm
                                                                                                                                                                                                                                          x19]))
                                                                                                                                                                                                                                 (Prelude.return
                                                                                                                                                                                                                                    (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                       x18
                                                                                                                                                                                                                                       x19))
                                                                                                                                                                                                                     DM.constructorHook
                                                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                                                             "FileGoodies"
                                                                                                                                                                                                                             0)
                                                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                                                             []
                                                                                                                                                                                                                             [DI.genTerm
                                                                                                                                                                                                                                x20,
                                                                                                                                                                                                                              DI.genTerm
                                                                                                                                                                                                                                x21]))
                                                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                                                          (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                             x20
                                                                                                                                                                                                                             x21))
                                                                                                                                                                                                           DM.constructorHook
                                                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                                                   "FileGoodies"
                                                                                                                                                                                                                   0)
                                                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                                                   []
                                                                                                                                                                                                                   [DI.genTerm
                                                                                                                                                                                                                      x22,
                                                                                                                                                                                                                    DI.genTerm
                                                                                                                                                                                                                      x23]))
                                                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                                                (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                   x22
                                                                                                                                                                                                                   x23))
                                                                                                                                                                                                 DM.constructorHook
                                                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                                                         "FileGoodies"
                                                                                                                                                                                                         0)
                                                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                                                         []
                                                                                                                                                                                                         [DI.genTerm
                                                                                                                                                                                                            x24,
                                                                                                                                                                                                          DI.genTerm
                                                                                                                                                                                                            x25]))
                                                                                                                                                                                                   (Prelude.return
                                                                                                                                                                                                      (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                         x24
                                                                                                                                                                                                         x25))
                                                                                                                                                                                       DM.constructorHook
                                                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                                                            (DI.SrcID
                                                                                                                                                                                               "FileGoodies"
                                                                                                                                                                                               0)
                                                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                                                               []
                                                                                                                                                                                               [DI.genTerm
                                                                                                                                                                                                  x26,
                                                                                                                                                                                                DI.genTerm
                                                                                                                                                                                                  x27]))
                                                                                                                                                                                         (Prelude.return
                                                                                                                                                                                            (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                               x26
                                                                                                                                                                                               x27))
                                                                                                                                                                             DM.constructorHook
                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                     "FileGoodies"
                                                                                                                                                                                     0)
                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                     []
                                                                                                                                                                                     [DI.genTerm
                                                                                                                                                                                        x28,
                                                                                                                                                                                      DI.genTerm
                                                                                                                                                                                        x29]))
                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                  (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                     x28
                                                                                                                                                                                     x29))
                                                                                                                                                                   DM.constructorHook
                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                           "FileGoodies"
                                                                                                                                                                           0)
                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                           []
                                                                                                                                                                           [DI.genTerm
                                                                                                                                                                              x30,
                                                                                                                                                                            DI.genTerm
                                                                                                                                                                              x31]))
                                                                                                                                                                     (Prelude.return
                                                                                                                                                                        (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                           x30
                                                                                                                                                                           x31))
                                                                                                                                                         DM.constructorHook
                                                                                                                                                           (DI.DebugInfo
                                                                                                                                                              (DI.SrcID
                                                                                                                                                                 "FileGoodies"
                                                                                                                                                                 0)
                                                                                                                                                              (DI.DynamicInfo
                                                                                                                                                                 []
                                                                                                                                                                 [DI.genTerm
                                                                                                                                                                    x32,
                                                                                                                                                                  DI.genTerm
                                                                                                                                                                    x33]))
                                                                                                                                                           (Prelude.return
                                                                                                                                                              (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                 x32
                                                                                                                                                                 x33))
                                                                                                                                               DM.constructorHook
                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                    (DI.SrcID
                                                                                                                                                       "FileGoodies"
                                                                                                                                                       0)
                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                       []
                                                                                                                                                       [DI.genTerm
                                                                                                                                                          x34,
                                                                                                                                                        DI.genTerm
                                                                                                                                                          x35]))
                                                                                                                                                 (Prelude.return
                                                                                                                                                    (Curry.DebugModule.Prelude.Cons
                                                                                                                                                       x34
                                                                                                                                                       x35))
                                                                                                                                     DM.constructorHook
                                                                                                                                       (DI.DebugInfo
                                                                                                                                          (DI.SrcID
                                                                                                                                             "FileGoodies"
                                                                                                                                             0)
                                                                                                                                          (DI.DynamicInfo
                                                                                                                                             []
                                                                                                                                             [DI.genTerm
                                                                                                                                                x36,
                                                                                                                                              DI.genTerm
                                                                                                                                                x37]))
                                                                                                                                       (Prelude.return
                                                                                                                                          (Curry.DebugModule.Prelude.Cons
                                                                                                                                             x36
                                                                                                                                             x37))
                                                                                                                           DM.constructorHook
                                                                                                                             (DI.DebugInfo
                                                                                                                                (DI.SrcID
                                                                                                                                   "FileGoodies"
                                                                                                                                   0)
                                                                                                                                (DI.DynamicInfo
                                                                                                                                   []
                                                                                                                                   [DI.genTerm
                                                                                                                                      x38,
                                                                                                                                    DI.genTerm
                                                                                                                                      x39]))
                                                                                                                             (Prelude.return
                                                                                                                                (Curry.DebugModule.Prelude.Cons
                                                                                                                                   x38
                                                                                                                                   x39))
                                                                                                                 DM.constructorHook
                                                                                                                   (DI.DebugInfo
                                                                                                                      (DI.SrcID
                                                                                                                         "FileGoodies"
                                                                                                                         0)
                                                                                                                      (DI.DynamicInfo
                                                                                                                         []
                                                                                                                         [DI.genTerm
                                                                                                                            x40,
                                                                                                                          DI.genTerm
                                                                                                                            x41]))
                                                                                                                   (Prelude.return
                                                                                                                      (Curry.DebugModule.Prelude.Cons
                                                                                                                         x40
                                                                                                                         x41))
                                                                                                       DM.constructorHook
                                                                                                         (DI.DebugInfo
                                                                                                            (DI.SrcID
                                                                                                               "FileGoodies"
                                                                                                               0)
                                                                                                            (DI.DynamicInfo
                                                                                                               []
                                                                                                               [DI.genTerm
                                                                                                                  x42,
                                                                                                                DI.genTerm
                                                                                                                  x43]))
                                                                                                         (Prelude.return
                                                                                                            (Curry.DebugModule.Prelude.Cons
                                                                                                               x42
                                                                                                               x43))
                                                                                             DM.constructorHook
                                                                                               (DI.DebugInfo
                                                                                                  (DI.SrcID
                                                                                                     "FileGoodies"
                                                                                                     0)
                                                                                                  (DI.DynamicInfo
                                                                                                     []
                                                                                                     [DI.genTerm
                                                                                                        x44,
                                                                                                      DI.genTerm
                                                                                                        x45]))
                                                                                               (Prelude.return
                                                                                                  (Curry.DebugModule.Prelude.Cons
                                                                                                     x44
                                                                                                     x45))
                                                                                   DM.constructorHook
                                                                                     (DI.DebugInfo
                                                                                        (DI.SrcID
                                                                                           "FileGoodies"
                                                                                           0)
                                                                                        (DI.DynamicInfo
                                                                                           []
                                                                                           [DI.genTerm
                                                                                              x46,
                                                                                            DI.genTerm
                                                                                              x47]))
                                                                                     (Prelude.return
                                                                                        (Curry.DebugModule.Prelude.Cons
                                                                                           x46
                                                                                           x47))
                                                                         DM.constructorHook
                                                                           (DI.DebugInfo
                                                                              (DI.SrcID
                                                                                 "FileGoodies"
                                                                                 0)
                                                                              (DI.DynamicInfo []
                                                                                 [DI.genTerm x48,
                                                                                  DI.genTerm x49]))
                                                                           (Prelude.return
                                                                              (Curry.DebugModule.Prelude.Cons
                                                                                 x48
                                                                                 x49))
                                                               DM.constructorHook
                                                                 (DI.DebugInfo
                                                                    (DI.SrcID "FileGoodies" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x50,
                                                                        DI.genTerm x51]))
                                                                 (Prelude.return
                                                                    (Curry.DebugModule.Prelude.Cons
                                                                       x50
                                                                       x51))
                                                     x58 <- do x56 <- do x54 <- do x52 <- DM.funcCallHook
                                                                                            "pathSeparatorChar"
                                                                                            (DI.DebugInfo
                                                                                               (DI.SrcID
                                                                                                  "FileGoodies"
                                                                                                  0)
                                                                                               (DI.DynamicInfo
                                                                                                  []
                                                                                                  []))
                                                                                            strict_pathSeparatorChar
                                                                                   x53 <- DM.constructorHook
                                                                                            (DI.DebugInfo
                                                                                               (DI.SrcID
                                                                                                  "FileGoodies"
                                                                                                  0)
                                                                                               (DI.DynamicInfo
                                                                                                  []
                                                                                                  []))
                                                                                            (Prelude.return
                                                                                               Curry.DebugModule.Prelude.Nil)
                                                                                   DM.constructorHook
                                                                                     (DI.DebugInfo
                                                                                        (DI.SrcID
                                                                                           "FileGoodies"
                                                                                           0)
                                                                                        (DI.DynamicInfo
                                                                                           []
                                                                                           [DI.genTerm
                                                                                              x52,
                                                                                            DI.genTerm
                                                                                              x53]))
                                                                                     (Prelude.return
                                                                                        (Curry.DebugModule.Prelude.Cons
                                                                                           x52
                                                                                           x53))
                                                                         x55 <- Prelude.return x2
                                                                         DM.funcCallHook
                                                                           "intersperse"
                                                                           (DI.DebugInfo
                                                                              (DI.SrcID
                                                                                 "FileGoodies"
                                                                                 0)
                                                                              (DI.DynamicInfo []
                                                                                 [DI.genTerm x54,
                                                                                  DI.genTerm x55]))
                                                                           (Curry.DebugModule.List.strict_intersperse
                                                                              x54
                                                                              x55)
                                                               DM.funcCallHook "concat"
                                                                 (DI.DebugInfo
                                                                    (DI.SrcID "FileGoodies" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x56]))
                                                                 (Curry.DebugModule.Prelude.strict_concat
                                                                    x56)
                                                     DM.funcCallHook "++"
                                                       (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x57, DI.genTerm x58]))
                                                       (Curry.DebugModule.Prelude.op_PlusPlus x57
                                                          x58)
                                           DM.funcCallHook "++"
                                             (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x59, DI.genTerm x60]))
                                             (Curry.DebugModule.Prelude.op_PlusPlus x59 x60)
                                 DM.funcCallHook "++"
                                   (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x61, DI.genTerm x62]))
                                   (Curry.DebugModule.Prelude.op_PlusPlus x61 x62)
                       DM.funcCallHook "$"
                         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                            (DI.DynamicInfo [] [DI.genTerm x63, DI.genTerm x64]))
                         (Curry.DebugModule.Prelude.op_Dollar x63 x64)
             x66 <- Prelude.return
                      (PC.partCall1 (Curry.DebugModule.Prelude.term_strict_return [])
                         Curry.DebugModule.Prelude.strict_return)
             x67 <- Prelude.return x3
             DM.funcCallHook "maybe"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x65, DI.genTerm x66, DI.genTerm x67]))
               (Curry.DebugModule.Prelude.strict_maybe x65 x66 x67)))
x'xterm_strict_getFileInPath46_35lambda4 x1
  = DI.Term "getFileInPath._#lambda4" (DI.SrcID "FileGoodies" 0) x1
 
strict_replaceFileName ::
                       (DM.DM dm) =>
                         DM.Func dm
                           (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                           (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                           ->
                           Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                             dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_replaceFileName x1 x2
  = DM.eval
      (DM.funcDeclHook "replaceFileName"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (DM.letHook
            (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
            (do x3 <- do x6 <- Prelude.return x2
                         DM.funcCallHook "splitDirectoryBaseName"
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                              (DI.DynamicInfo [] [DI.genTerm x6]))
                           (strict_splitDirectoryBaseName x6)
                DM.eval
                  (DM.letHook
                     (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                     (do x4 <- do x7 <- Prelude.return x3
                                  DM.funcCallHook "replaceFileName._#selFP12#dir"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7]))
                                    (x'xstrict_replaceFileName46_35selFP1235dir x7)
                         DM.eval
                           (DM.letHook
                              (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                              (do x5 <- do x8 <- Prelude.return x3
                                           DM.funcCallHook "replaceFileName._#selFP13#fn"
                                             (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x8]))
                                             (x'xstrict_replaceFileName46_35selFP1335fn x8)
                                  DM.eval
                                    (do x9 <- Prelude.return x1
                                        x10 <- Prelude.return x5
                                        x11 <- Prelude.return x4
                                        DM.funcCallHook "_case_4"
                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                             (DI.DynamicInfo []
                                                [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11]))
                                          (strict__case_4 x9 x10 x11)))))))))
term_strict_replaceFileName x1
  = DI.Term "replaceFileName" (DI.SrcID "FileGoodies" 0) x1
 
x'xstrict_replaceFileName46_35selFP1235dir ::
                                           (DM.DM dm) =>
                                             Curry.DebugModule.Prelude.Tuple2
                                               (Curry.DebugModule.Prelude.List
                                                  Curry.DebugModule.Prelude.Char)
                                               (Curry.DebugModule.Prelude.List
                                                  Curry.DebugModule.Prelude.Char)
                                               ->
                                               dm
                                                 (Curry.DebugModule.Prelude.List
                                                    Curry.DebugModule.Prelude.Char)
x'xstrict_replaceFileName46_35selFP1235dir x1
  = DM.eval
      (DM.funcDeclHook "replaceFileName._#selFP12#dir"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_1"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_1 x2)))
x'xterm_strict_replaceFileName46_35selFP1235dir x1
  = DI.Term "replaceFileName._#selFP12#dir"
      (DI.SrcID "FileGoodies" 0)
      x1
 
x'xstrict_replaceFileName46_35selFP1335fn ::
                                          (DM.DM dm) =>
                                            Curry.DebugModule.Prelude.Tuple2
                                              (Curry.DebugModule.Prelude.List
                                                 Curry.DebugModule.Prelude.Char)
                                              (Curry.DebugModule.Prelude.List
                                                 Curry.DebugModule.Prelude.Char)
                                              ->
                                              dm
                                                (Curry.DebugModule.Prelude.List
                                                   Curry.DebugModule.Prelude.Char)
x'xstrict_replaceFileName46_35selFP1335fn x1
  = DM.eval
      (DM.funcDeclHook "replaceFileName._#selFP13#fn"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_0"
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_0 x2)))
x'xterm_strict_replaceFileName46_35selFP1335fn x1
  = DI.Term "replaceFileName._#selFP13#fn" (DI.SrcID "FileGoodies" 0)
      x1
strict__case_0 x1
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_0
                           x4)))
term_strict__case_0 x1
  = DI.Term "_case_0" (DI.SrcID "FileGoodies" 0) x1
strict__case_1 x1
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_1
                           x4)))
term_strict__case_1 x1
  = DI.Term "_case_1" (DI.SrcID "FileGoodies" 0) x1
strict__case_4 x1 x5 x4
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x5, DI.genTerm x4]))
         (do x22 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x22]))
               (case x22 of
                    Curry.DebugModule.Prelude.Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x1
                                  x11 <- Prelude.return x4
                                  x12 <- Prelude.return x5
                                  x13 <- Prelude.return x6
                                  x14 <- Prelude.return x7
                                  x15 <- do x8 <- Prelude.return x6
                                            x9 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return
                                                       (Curry.DebugModule.Prelude.Char '.'))
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (Curry.DebugModule.Prelude.op_EqEq x8 x9)
                                  DM.funcCallHook "_case_3"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x10, DI.genTerm x11, DI.genTerm x12,
                                           DI.genTerm x13, DI.genTerm x14, DI.genTerm x15]))
                                    (strict__case_3 x10 x11 x12 x13 x14 x15)))
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x20 <- Prelude.return x4
                                  x21 <- do x18 <- DM.funcCallHook "separatorChar"
                                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                        (DI.DynamicInfo [] []))
                                                     strict_separatorChar
                                            x19 <- do x16 <- Prelude.return x1
                                                      x17 <- Prelude.return x5
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x16, DI.genTerm x17]))
                                                        (Curry.DebugModule.Prelude.strict_apply x16
                                                           x17)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x18, DI.genTerm x19]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x18 x19))
                                  DM.funcCallHook "++"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x20, DI.genTerm x21]))
                                    (Curry.DebugModule.Prelude.op_PlusPlus x20 x21)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x22])))
                           (strict__case_4 x1 x5)
                           x22)))
term_strict__case_4 x1
  = DI.Term "_case_4" (DI.SrcID "FileGoodies" 0) x1
strict__case_3 x1 x4 x5 x6 x7 x8
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6,
                DI.genTerm x7, DI.genTerm x8]))
         (do x19 <- Prelude.return x8
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x19]))
               (case x19 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- Prelude.return x4
                                  x11 <- Prelude.return x5
                                  x12 <- Prelude.return x7
                                  DM.funcCallHook "_case_2"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11,
                                           DI.genTerm x12]))
                                    (strict__case_2 x9 x10 x11 x12)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x17 <- Prelude.return x4
                                  x18 <- do x15 <- DM.funcCallHook "separatorChar"
                                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                        (DI.DynamicInfo [] []))
                                                     strict_separatorChar
                                            x16 <- do x13 <- Prelude.return x1
                                                      x14 <- Prelude.return x5
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x13, DI.genTerm x14]))
                                                        (Curry.DebugModule.Prelude.strict_apply x13
                                                           x14)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x15, DI.genTerm x16]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x15 x16))
                                  DM.funcCallHook "++"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x17, DI.genTerm x18]))
                                    (Curry.DebugModule.Prelude.op_PlusPlus x17 x18)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x19])))
                           (strict__case_3 x1 x4 x5 x6 x7)
                           x19)))
term_strict__case_3 x1
  = DI.Term "_case_3" (DI.SrcID "FileGoodies" 0) x1
strict__case_2 x1 x4 x5 x7
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x7]))
         (do x18 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x18]))
               (case x18 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x1
                                  x11 <- Prelude.return x5
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (Curry.DebugModule.Prelude.strict_apply x10 x11)))
                    Curry.DebugModule.Prelude.Cons x8 x9
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x16 <- Prelude.return x4
                                  x17 <- do x14 <- DM.funcCallHook "separatorChar"
                                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                        (DI.DynamicInfo [] []))
                                                     strict_separatorChar
                                            x15 <- do x12 <- Prelude.return x1
                                                      x13 <- Prelude.return x5
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x12, DI.genTerm x13]))
                                                        (Curry.DebugModule.Prelude.strict_apply x12
                                                           x13)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x14, DI.genTerm x15]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x14 x15))
                                  DM.funcCallHook "++"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x16, DI.genTerm x17]))
                                    (Curry.DebugModule.Prelude.op_PlusPlus x16 x17)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x18])))
                           (strict__case_2 x1 x4 x5)
                           x18)))
term_strict__case_2 x1
  = DI.Term "_case_2" (DI.SrcID "FileGoodies" 0) x1
strict__case_5 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_5"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x18 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x18]))
               (case x18 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nothing)
                                  DM.funcCallHook "return"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Curry.DebugModule.Prelude.strict_return x6)))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x16 <- do x11 <- do x9 <- Prelude.return x4
                                                      x10 <- do x7 <- DM.funcCallHook
                                                                        "separatorChar"
                                                                        (DI.DebugInfo
                                                                           (DI.SrcID "FileGoodies"
                                                                              0)
                                                                           (DI.DynamicInfo [] []))
                                                                        strict_separatorChar
                                                                x8 <- Prelude.return x1
                                                                DM.constructorHook
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "FileGoodies" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x7,
                                                                         DI.genTerm x8]))
                                                                  (Prelude.return
                                                                     (Curry.DebugModule.Prelude.Cons
                                                                        x7
                                                                        x8))
                                                      DM.funcCallHook "++"
                                                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10]))
                                                        (Curry.DebugModule.Prelude.op_PlusPlus x9
                                                           x10)
                                            x12 <- Prelude.return x2
                                            DM.funcCallHook
                                              "lookupFileInPath.lookupFirstFileWithSuffix.36"
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12]))
                                              (x'xstrict_lookupFileInPath46lookupFirstFileWithSuffix4636
                                                 x11
                                                 x12)
                                  x17 <- do x13 <- Prelude.return x5
                                            x14 <- Prelude.return x1
                                            x15 <- Prelude.return x2
                                            Prelude.return
                                              (PC.partCall1
                                                 (x'xterm_strict_lookupFileInPath46lookupFirstFile463646_35lambda2
                                                    [DI.genTerm x13, DI.genTerm x14,
                                                     DI.genTerm x15])
                                                 (x'xstrict_lookupFileInPath46lookupFirstFile463646_35lambda2
                                                    x13
                                                    x14
                                                    x15))
                                  DM.funcCallHook ">>="
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x16, DI.genTerm x17]))
                                    (Curry.DebugModule.Prelude.op_GtGtEq x16 x17)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x18])))
                           (strict__case_5 x1 x2)
                           x18)))
term_strict__case_5 x1
  = DI.Term "_case_5" (DI.SrcID "FileGoodies" 0) x1
strict__case_6 x1 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_6"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x9 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- do x5 <- Prelude.return x2
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (Prelude.return (Curry.DebugModule.Prelude.Just x5))
                                  DM.funcCallHook "return"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Curry.DebugModule.Prelude.strict_return x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x3
                                  DM.funcCallHook "lookupFileInPath.lookupFirstFileWithSuffix.36"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (x'xstrict_lookupFileInPath46lookupFirstFileWithSuffix4636 x7
                                       x8)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           (strict__case_6 x1 x2 x3)
                           x9)))
term_strict__case_6 x1
  = DI.Term "_case_6" (DI.SrcID "FileGoodies" 0) x1
strict__case_7 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_7"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x15 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nothing)
                                  DM.funcCallHook "return"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Curry.DebugModule.Prelude.strict_return x6)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x5 <- do x7 <- Prelude.return x1
                                              x8 <- Prelude.return x3
                                              DM.funcCallHook "++"
                                                (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x7, DI.genTerm x8]))
                                                (Curry.DebugModule.Prelude.op_PlusPlus x7 x8)
                                     DM.eval
                                       (do x13 <- do x9 <- Prelude.return x5
                                                     DM.funcCallHook "doesFileExist"
                                                       (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x9]))
                                                       (Curry.DebugModule.Directory.strict_doesFileExist
                                                          x9)
                                           x14 <- do x10 <- Prelude.return x1
                                                     x11 <- Prelude.return x5
                                                     x12 <- Prelude.return x4
                                                     Prelude.return
                                                       (PC.partCall1
                                                          (x'xterm_strict_lookupFileInPath46lookupFirstFileWithSuffix463646_35lambda3
                                                             [DI.genTerm x10, DI.genTerm x11,
                                                              DI.genTerm x12])
                                                          (x'xstrict_lookupFileInPath46lookupFirstFileWithSuffix463646_35lambda3
                                                             x10
                                                             x11
                                                             x12))
                                           DM.funcCallHook ">>="
                                             (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x13, DI.genTerm x14]))
                                             (Curry.DebugModule.Prelude.op_GtGtEq x13 x14)))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_7 x1)
                           x15)))
term_strict__case_7 x1
  = DI.Term "_case_7" (DI.SrcID "FileGoodies" 0) x1
strict__case_8 x1 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_8"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x10 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- Prelude.return x2
                                  DM.funcCallHook "lookupFileInPath.lookupFirstFileWithSuffix.36"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (x'xstrict_lookupFileInPath46lookupFirstFileWithSuffix4636 x5
                                       x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x2
                                  x9 <- Prelude.return x3
                                  DM.funcCallHook "lookupFileInPath.lookupFirstFile.36"
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
                                    (x'xstrict_lookupFileInPath46lookupFirstFile4636 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_8 x1 x2 x3)
                           x10)))
term_strict__case_8 x1
  = DI.Term "_case_8" (DI.SrcID "FileGoodies" 0) x1
strict__case_9 x1
  = DM.eval
      (DM.funcDeclHook "_case_9"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_9
                           x4)))
term_strict__case_9 x1
  = DI.Term "_case_9" (DI.SrcID "FileGoodies" 0) x1
strict__case_10 x1
  = DM.eval
      (DM.funcDeclHook "_case_10"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_10
                           x4)))
term_strict__case_10 x1
  = DI.Term "_case_10" (DI.SrcID "FileGoodies" 0) x1
strict__case_12 x1
  = DM.eval
      (DM.funcDeclHook "_case_12"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x20 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x4 <- do x12 <- do x9 <- do x7 <- Prelude.return
                                                                         (PC.partCall2
                                                                            (Curry.DebugModule.Prelude.term_op_EqEq
                                                                               [])
                                                                            Curry.DebugModule.Prelude.op_EqEq)
                                                                 x8 <- DM.funcCallHook
                                                                         "pathSeparatorChar"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "FileGoodies"
                                                                               0)
                                                                            (DI.DynamicInfo [] []))
                                                                         strict_pathSeparatorChar
                                                                 Prelude.return
                                                                   (PC.partCall1
                                                                      (Curry.DebugModule.Prelude.term_strict_flip
                                                                         [DI.genTerm x7,
                                                                          DI.genTerm x8])
                                                                      (Curry.DebugModule.Prelude.strict_flip
                                                                         x7
                                                                         x8))
                                                        DM.funcCallHook "break"
                                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                             (DI.DynamicInfo [] [DI.genTerm x9]))
                                                          (Curry.DebugModule.Prelude.strict_break
                                                             x9)
                                              x13 <- do x10 <- Prelude.return x2
                                                        x11 <- Prelude.return x3
                                                        DM.constructorHook
                                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                             (DI.DynamicInfo []
                                                                [DI.genTerm x10, DI.genTerm x11]))
                                                          (Prelude.return
                                                             (Curry.DebugModule.Prelude.Cons x10
                                                                x11))
                                              DM.funcCallHook "apply"
                                                (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x12, DI.genTerm x13]))
                                                (Curry.DebugModule.Prelude.strict_apply x12 x13)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x5 <- do x14 <- Prelude.return x4
                                                       DM.funcCallHook "splitPath._#selFP9#ys"
                                                         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x14]))
                                                         (x'xstrict_splitPath46_35selFP935ys x14)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x6 <- do x15 <- Prelude.return x4
                                                                DM.funcCallHook
                                                                  "splitPath._#selFP10#zs"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "FileGoodies" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x15]))
                                                                  (x'xstrict_splitPath46_35selFP1035zs
                                                                     x15)
                                                       DM.eval
                                                         (do x17 <- Prelude.return x5
                                                             x18 <- Prelude.return x6
                                                             x19 <- do x16 <- Prelude.return x6
                                                                       DM.funcCallHook "null"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "FileGoodies"
                                                                               0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x16]))
                                                                         (Curry.DebugModule.Prelude.strict_null
                                                                            x16)
                                                             DM.funcCallHook "_case_11"
                                                               (DI.DebugInfo
                                                                  (DI.SrcID "FileGoodies" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x17,
                                                                      DI.genTerm x18,
                                                                      DI.genTerm x19]))
                                                               (strict__case_11 x17 x18 x19)))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           strict__case_12
                           x20)))
term_strict__case_12 x1
  = DI.Term "_case_12" (DI.SrcID "FileGoodies" 0) x1
strict__case_11 x5 x6 x7
  = DM.eval
      (DM.funcDeclHook "_case_11"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
         (do x14 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x5
                                  x9 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x8 x9))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x5
                                  x13 <- do x11 <- do x10 <- Prelude.return x6
                                                      DM.funcCallHook "tail"
                                                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x10]))
                                                        (Curry.DebugModule.Prelude.strict_tail x10)
                                            DM.funcCallHook "splitPath"
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x11]))
                                              (strict_splitPath x11)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x12 x13))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_11 x5 x6)
                           x14)))
term_strict__case_11 x1
  = DI.Term "_case_11" (DI.SrcID "FileGoodies" 0) x1
strict__case_13 x1
  = DM.eval
      (DM.funcDeclHook "_case_13"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_13
                           x4)))
term_strict__case_13 x1
  = DI.Term "_case_13" (DI.SrcID "FileGoodies" 0) x1
strict__case_14 x1
  = DM.eval
      (DM.funcDeclHook "_case_14"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_14
                           x4)))
term_strict__case_14 x1
  = DI.Term "_case_14" (DI.SrcID "FileGoodies" 0) x1
strict__case_15 x1 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_15"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x15 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x6 x7))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- do x9 <- DM.funcCallHook "reverse"
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo [] []))
                                                    Curry.DebugModule.Prelude.strict_reverse
                                            x10 <- do x8 <- Prelude.return x4
                                                      DM.funcCallHook "tail"
                                                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x8]))
                                                        (Curry.DebugModule.Prelude.strict_tail x8)
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (Curry.DebugModule.Prelude.strict_apply x9 x10)
                                  x14 <- do x11 <- DM.funcCallHook "reverse"
                                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                        (DI.DynamicInfo [] []))
                                                     Curry.DebugModule.Prelude.strict_reverse
                                            x12 <- Prelude.return x3
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12]))
                                              (Curry.DebugModule.Prelude.strict_apply x11 x12)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x13, DI.genTerm x14]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x13 x14))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_15 x1 x3 x4)
                           x15)))
term_strict__case_15 x1
  = DI.Term "_case_15" (DI.SrcID "FileGoodies" 0) x1
strict__case_16 x1
  = DM.eval
      (DM.funcDeclHook "_case_16"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_16
                           x4)))
term_strict__case_16 x1
  = DI.Term "_case_16" (DI.SrcID "FileGoodies" 0) x1
strict__case_17 x1
  = DM.eval
      (DM.funcDeclHook "_case_17"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_17
                           x4)))
term_strict__case_17 x1
  = DI.Term "_case_17" (DI.SrcID "FileGoodies" 0) x1
strict__case_18 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_18"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x19 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                  (DI.DynamicInfo [] [DI.genTerm x19]))
               (case x19 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- do x6 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return
                                                       (Curry.DebugModule.Prelude.Char '.'))
                                            x7 <- DM.constructorHook
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return Curry.DebugModule.Prelude.Nil)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x6 x7))
                                  x11 <- do x8 <- DM.funcCallHook "reverse"
                                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                       (DI.DynamicInfo [] []))
                                                    Curry.DebugModule.Prelude.strict_reverse
                                            x9 <- Prelude.return x3
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (Curry.DebugModule.Prelude.strict_apply x8 x9)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x10 x11))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x17 <- do x13 <- DM.funcCallHook "reverse"
                                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                        (DI.DynamicInfo [] []))
                                                     Curry.DebugModule.Prelude.strict_reverse
                                            x14 <- do x12 <- Prelude.return x4
                                                      DM.funcCallHook "tail"
                                                        (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x12]))
                                                        (Curry.DebugModule.Prelude.strict_tail x12)
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x13, DI.genTerm x14]))
                                              (Curry.DebugModule.Prelude.strict_apply x13 x14)
                                  x18 <- do x15 <- DM.funcCallHook "reverse"
                                                     (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                        (DI.DynamicInfo [] []))
                                                     Curry.DebugModule.Prelude.strict_reverse
                                            x16 <- Prelude.return x3
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x15, DI.genTerm x16]))
                                              (Curry.DebugModule.Prelude.strict_apply x15 x16)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x17, DI.genTerm x18]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x17 x18))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x19])))
                           (strict__case_18 x3 x4)
                           x19)))
term_strict__case_18 x1
  = DI.Term "_case_18" (DI.SrcID "FileGoodies" 0) x1
strict__case_19 x1
  = DM.eval
      (DM.funcDeclHook "_case_19"
         (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x6 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "FileGoodies" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x2
                                  x5 <- DM.funcCallHook "separatorChar"
                                          (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                             (DI.DynamicInfo [] []))
                                          strict_separatorChar
                                  DM.funcCallHook "=="
                                    (DI.DebugInfo (DI.SrcID "FileGoodies" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (Curry.DebugModule.Prelude.op_EqEq x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "FileGoodies" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           strict__case_19
                           x6)))
term_strict__case_19 x1
  = DI.Term "_case_19" (DI.SrcID "FileGoodies" 0) x1