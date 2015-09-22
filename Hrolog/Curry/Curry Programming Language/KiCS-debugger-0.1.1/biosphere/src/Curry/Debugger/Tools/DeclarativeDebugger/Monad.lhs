> {-# OPTIONS_GHC -fglasgow-exts #-}
> module Curry.Debugger.Tools.DeclarativeDebugger.Monad where

> import Control.Monad.Error
> import Control.Monad.State
> import Control.Monad.Trans

> import Curry.Debugger.DebugMonad
> import Curry.Debugger.Oracle
> import Curry.Debugger.DebugInfo
> import Curry.Debugger.DebugInfoGoodies
> import Curry.Debugger.ShowTerm

> import qualified Curry.Debugger.Tools.DeclarativeDebugger.CallStack as CS
> import Curry.Debugger.Tools.DeclarativeDebugger.CallStack ( CallStack
>                  , FuncCall (..)
>                  )

> import qualified Curry.Debugger.Tools.DeclarativeDebugger.Ratings as R
> import Curry.Debugger.Tools.DeclarativeDebugger.Ratings ( Ratings (..)
>                , Rating (..)
>                )

> import qualified Curry.Debugger.Tools.DeclarativeDebugger.UI as UI
> import Curry.Debugger.Tools.DeclarativeDebugger.UI ( Cmd (..)
>           )

> type DDM a = DMT DeclarativeState (ErrorT DeclarativeException IO) a

> data DeclarativeException = QuitException    DeclarativeState
>                           | RestartException DeclarativeState
>                           | UndefinedException
>  deriving Show

> instance Error DeclarativeException where
>     noMsg = UndefinedException

> data DeclarativeState = DS { execMode :: ExecMode
>                            , ratings :: Ratings
>                            , unratedLeft :: Bool
>                            , wrongRules :: [FuncCall]
>                            , nextCmd :: Maybe Cmd
>                            , undoStack :: [Cmd]
>                            , replayStack :: [Cmd]
>                            , callStack :: CallStack
>                            }
>  deriving Show

> type InternalState = (Oracle, DeclarativeState)

> data ExecMode = ResultMode  -- just get the result
>               | CorrectMode -- current subcomputation is marked as correct
>               | InspectMode
>               | NavigateMode
>  deriving Show

> instance DM (DMT DeclarativeState (ErrorT DeclarativeException IO)) where
>     funcDeclHook fname info ma = do
>         emode <- getExecMode

>         r <- case emode of
>             ResultMode   -> evaluateResult ma
>             CorrectMode  -> ma
>             NavigateMode -> navigate (niceFname fname) info ma
>             InspectMode  -> inspect (niceFname fname) info ma
>         return r

> niceFname :: String -> String
> niceFname fname =
>     let c = head fname
>     in  if c < 'a' || c > 'z' then
>             '(' : fname ++ ")"
>         else
>             fname

> evaluateResult :: DDM a -> DDM a
> evaluateResult ma = do
>     rating <- getRating
>     let iscorrect = rating == Correct

>     when iscorrect $ setExecMode CorrectMode
>     shiftRatings
>     r <- ma
>     when iscorrect $ setExecMode ResultMode

>     return r

> navigate :: GenTerm a => String -> DI -> DDM a -> DDM a
> navigate fname info ma = do
>     setUnratedLeft

	>     (modname, fname) <- liftIO $ getQualifiedFuncName info

>     modname <- getModName

>     let cv = map (showTerm) $ currentValues $ dynamicInfo info
>     rating <- getRating
>     let fc = FC modname fname cv Nothing rating
>     csPush fc 
>     let sfc = show fc

>     r <- navigatePrompt sfc fname info ma

>     csPop

>     return r

> navigatePrompt :: GenTerm a => String -> String -> DI -> DDM a -> DDM a
> navigatePrompt p fname info ma = do
>     replaying <- isReplaying
>     nc <- getNextCmd
>     cmd <-
>         case nc of
>             Nothing ->
>                 if replaying then 
>                     popReplayStack
>                 else
>                     liftIO $ UI.getNavigateCmd p
>             Just c -> do
>                 setNextCmd Nothing
>                 return c

>     case cmd of
>         HelpCmd -> do
>             liftIO UI.navigateHelp
>             navigatePrompt p fname info ma

>         QuitCmd -> do
>             quit

>         InspectCmd -> do
>             pushUndoStack cmd
>             setExecMode InspectMode
>             csPop
>             inspect fname info ma

>         StackCmd -> do
>             csPrint
>             navigatePrompt p fname info ma

>         RestartCmd -> do
>             pushUndoStack cmd
>             restart

>         StepCmd n -> do
>             when (nc == Nothing) $ pushUndoStack cmd
>             when (n > 1) $ setNextCmd $ Just $ StepCmd $ n - 1
>             shiftRatings
>             ma

>         SkipCmd n -> do
>             when (nc == Nothing) $ pushUndoStack cmd
>             when (n > 1) $ setNextCmd $ Just $ SkipCmd $ n - 1
>             setExecMode ResultMode
>             r <- evaluateResult ma
>             setExecMode NavigateMode
>             return r

>         UndoCmd n -> do
>             prepareReplay n
>             restart

>         _ -> do
>             liftIO $ do
>                 putStr "Error: Unexpected command "
>                 print cmd
>             navigatePrompt p fname info ma

> inspect :: GenTerm a => String -> DI -> DDM a -> DDM a
> inspect fname info ma = do
>     rating <- getRating

>     case rating of
>         Correct -> do
>             setExecMode CorrectMode
>             r <- ma
>             setExecMode InspectMode
>             return r

>         Wrong -> do
>             shiftRatings
>             ma

>         Unrated -> do
>             setUnratedLeft

	>             (modname, fname) <- liftIO $ getQualifiedFuncName info

>             modname <- getModName

>             let cv = map (showTerm) $ currentValues $ dynamicInfo info

>             state <- backupState
>             setExecMode ResultMode
>             res <- evaluateResult ma
>             setExecMode InspectMode
>             state' <- backupState
>             restoreState state
>             let sr = showGenTerm res

>             let fc = FC modname fname cv (Just sr) rating
>             csPush fc 
>             let sfc = show fc

>             r <- inspectPrompt sfc state' res info ma

>             csPop

>             return r

> loopWrong :: DDM a -> DDM a
> loopWrong ma = do
>     (o, dds)  <- backupState
>     r <- ma
>     (_, dds') <- backupState

>     case unratedLeft dds' of
>         True -> do
>             let state' = (o, dds { ratings = R.restart $ ratings dds'
>                                  , unratedLeft = False
>                                  }
>                          )
>             restoreState state'
>             liftIO $ UI.putEndMsg []
>             loopWrong ma

>         False -> do
>             liftIO $ do
>                 putStrLn "Nothing left for rating."
>             quit

> inspectPrompt :: String -> InternalState -> a -> DI -> DDM a -> DDM a
> inspectPrompt sfc state' res info ma = do
>     replaying <- isReplaying
>     nc <- getNextCmd

>     cmd <-
>         case nc of
>             Nothing ->
>                 if replaying then 
>                     popReplayStack
>                 else
>                     liftIO $ UI.getInspectCmd sfc
>             Just c -> do
>                 setNextCmd Nothing
>                 return c

>     case cmd of
>         HelpCmd -> do
>             liftIO UI.inspectHelp
>             inspectPrompt sfc state' res info ma

>         QuitCmd -> do
>             quit

>         RestartCmd -> do
>             pushUndoStack cmd
>             restart

>         StackCmd -> do
>             csPrint
>             inspectPrompt sfc state' res info ma

>         RateCmd Correct -> do
>             pushUndoStack cmd
>             modifyRatings $
>                 \(Ratings p _) -> Ratings (R.push p 1 Correct)
>                                           (R.future $ ratings $ snd state')
>             setExecMode CorrectMode
>             r <- ma
>             setExecMode InspectMode
>             return r

>         RateCmd Wrong -> do
>             pushUndoStack cmd
>             modifyRatings $
>                 \(Ratings _ f) -> Ratings [] f
>             fc <- csTop
>             let fc' = fc { rating = Wrong }
>             csPop
>             csPush fc'
>             addWrongRule fc'
>             loopWrong ma

>         SkipCmd n -> do
>             when (nc == Nothing) $ pushUndoStack cmd
>             when (n > 1) $ setNextCmd $ Just $ SkipCmd $ n - 1
>             shiftRatings
>             ma

>         UndoCmd n -> do
>             prepareReplay n
>             restart

>         _ -> do
>             liftIO $ do
>                 putStr "Error: Unexpected command "
>                 print cmd
>             inspectPrompt sfc state' res info ma

State handling functions
========================

> backupState :: DDM InternalState
> backupState = do
>     o   <- getOracle
>     dds <- getToolState
>     return (o, dds)

> restoreState :: InternalState -> DDM ()
> restoreState (o, dds) = do
>     setOracle o
>     putToolState dds

> printState :: DDM ()
> printState = backupState >>= liftIO . print

Ratings
=======

> setUnratedLeft :: DDM ()
> setUnratedLeft = modifyToolState $ \s -> s { unratedLeft = True } 

> addWrongRule :: FuncCall -> DDM ()
> addWrongRule fc = modifyToolState $ \s -> s { wrongRules = fc : wrongRules s }

> shiftRatings :: DDM ()
> shiftRatings = modifyToolState $ \s -> s { ratings = R.shift (ratings s) 1 }

> modifyRatings :: (Ratings -> Ratings) -> DDM ()
> modifyRatings f = modifyToolState $ \s -> s { ratings = f (ratings s) }

> getRating :: DDM Rating
> getRating = do
>     st <- getToolState
>     return $ R.current $ ratings st

Stepping, skipping and undoing
==============================

> setNextCmd :: Maybe Cmd -> DDM ()
> setNextCmd mnc = modifyToolState $ \s -> s { nextCmd = mnc }

> getNextCmd :: DDM (Maybe Cmd)
> getNextCmd = do
>     st <- getToolState
>     return $ nextCmd st

> pushUndoStack :: Cmd -> DDM ()
> pushUndoStack c = modifyToolState $ \s -> s { undoStack = c : undoStack s }

> prepareReplay :: Int -> DDM ()
> prepareReplay n = modifyToolState $ \s -> s { replayStack = reverse $
>                                                                drop n $
>                                                                    undoStack s
>                                             , undoStack = []
>                                             }

> isReplaying :: DDM Bool
> isReplaying = do
>     st <- getToolState
>     return $ [] /= replayStack st

> popReplayStack :: DDM Cmd
> popReplayStack = do
>     st <- getToolState
>     let (c : cs) = replayStack st
>     modifyToolState $ \s -> s { replayStack = cs }
>     return c

CallStack
=========

> csPush :: FuncCall -> DDM ()
> csPush fc =
>     modifyToolState $
>         \s -> s { callStack = CS.push (callStack s) fc }

> csPop :: DDM ()
> csPop =
>     modifyToolState $
>         \s -> s { callStack = CS.pop $ callStack s }

> csTop :: DDM FuncCall
> csTop = do
>     st <- getToolState
>     return $ CS.top $ callStack st

> csPrint :: DDM ()
> csPrint = do
>     st <- getToolState
>     liftIO $ print $ callStack st

ExecMode
========

> getExecMode = do
>     st <- getToolState
>     return $ execMode st

> setExecMode :: ExecMode -> DDM ()
> setExecMode em = modifyToolState $ \s -> s { execMode = em }

Execeptions
===========

> restart :: DDM a
> restart = do
>     st <- getToolState
>     lift $ throwError $ RestartException st

> quit :: DDM a
> quit = do
>     st <- getToolState
>     lift $ throwError $ QuitException st

Running the debugger
====================

> debugThis :: GenTerm a => DDM a -> DDM (a, DeclarativeState)
> debugThis exp = do
>     r <- exp
>     st <- getToolState
>     return (r, st)

> debugLoop :: GenTerm a => DDM a -> String -> ExecMode -> Ratings
>                                 -> [FuncCall] -> [Cmd] -> [Cmd] -> IO ()
> debugLoop exp sfile emode rats wrongs undos replays = do
>     er <- runErrorT $ debugDMT (debugThis exp) sfile $
>         DS { execMode    = emode
>            , ratings     = rats
>            , unratedLeft = False
>            , undoStack   = undos
>            , replayStack = replays
>            , nextCmd     = Nothing
>            , wrongRules  = wrongs
>            , callStack   = CS.new
>            }

>     case er of
>         Left e ->
>             case e of
>                 QuitException st -> do
>                     liftIO $ mapM_ print $ wrongRules st
>                     return ()
>                 RestartException st ->
>                     debugLoop exp
>                               sfile 
>                               NavigateMode
>                               R.new
>                               []
>                               (undoStack   st)
>                               (replayStack st)
>                 UndefinedException -> do
>                     putStrLn "This should not happen. Please report bug."
>         Right (r, st)
>             | unratedLeft st -> do
>                 UI.putEndMsg [ ("Result", showGenTerm r)
> --                             , ("Oracle", show $ oracle dms)
>                              ]
>                 debugLoop exp
>                           sfile
>                           (execMode st)
>                           (R.restart $ ratings st)
>                           (wrongRules st)
>                           (undoStack st)
>                           (replayStack st)
>             | otherwise -> do
>                 liftIO $ do
>                     putStrLn "Nothing left for rating."
>                     mapM_ print $ wrongRules st
>                 return ()

> run :: GenTerm a => DDM a -> String -> IO ()
> run exp sfile = do
>     UI.start
>     debugLoop exp sfile NavigateMode R.new [] [] []
>     UI.stop
>     putStrLn "Bye."

> getModName = return "" -- FIXME

