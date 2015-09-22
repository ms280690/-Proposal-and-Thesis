> {-# OPTIONS -cpp #-} 
> module Curry.Debugger.Tools.DeclarativeDebugger.UI ( Cmd (..)
>           , start
>           , stop
>           , navigateHelp
>           , inspectHelp
>           , getNavigateCmd
>           , getInspectCmd
>           , putLabledInfos
>           , putEndMsg
>           )
>  where

> import Data.List (isPrefixOf)

> import qualified System.Console.Readline as RL

> import Curry.Debugger.Tools.DeclarativeDebugger.Ratings ( Ratings
>                , Rating (..)
>                )

> data Cmd = StackCmd
>          | HelpCmd
>          | InspectCmd
>          | StepCmd Int
>          | UndoCmd Int
>          | SkipCmd Int
>          | QuitCmd
>          | RateCmd Rating
>          | RestartCmd
>  deriving (Eq, Show)

> type Commands = [(String, String, [[ArgType]], GenCmd)]

> data ArgType = SpecialArgType String
>              | NaturalIntArgType

> data Arg = SpecialArg String
>          | IntArg Int

> type GenCmd = [Arg] -> Cmd

> navigateCommands :: Commands
> navigateCommands =
>     [ ( "help"
>       , "Shows these information."
>       , []
>       , \_ -> HelpCmd
>       )
>     , ( "quit"
>       , "Quit debugger."
>       , []
>       , \_ -> QuitCmd
>       )
>     , ( "restart"
>       , "Restarts the whole computation."
>       , []
>       , \_ -> RestartCmd
>       )
>     , ( "inspect"
>       , "Go to inspection mode."
>       , []
>       , \_ -> InspectCmd
>       )
>     , ( "step"
>       , "Step into current evaluation, with positive n do n steps."
>       , [ []
>         , [NaturalIntArgType]
>         ]
>       , \args -> case args of
>                      []         -> StepCmd 1
>                      [IntArg n] -> StepCmd n
>       )
>     , ( "skip"
>       , "Skip this expression, with positive n the next n expressions."
>       , [ []
>         , [NaturalIntArgType]
>         ]
>       , \args -> case args of
>                      []         -> SkipCmd 1
>                      [IntArg n] -> SkipCmd n
>       )
>     , ( "undo"
>       , "Undo the previous command, with positive n the n commands "
>      ++ "executed before."
>       , [ []
>         , [NaturalIntArgType]
>         ]
>       , \args -> case args of
>                      []         -> UndoCmd 1
>                      [IntArg n] -> UndoCmd n
>       )
>     , ( "stack"
>       , "Show callstack."
>       , []
>       , \_ -> StackCmd
>       )
>     ]

> inspectCommands :: Commands
> inspectCommands =
>     [ ( "help"
>       , "Shows these information."
>       , []
>       , \_ -> HelpCmd
>       )
>     , ( "quit"
>       , "Quit debugger."
>       , []
>       , \_ -> QuitCmd
>       )
>     , ( "restart"
>       , "Restart the whole debugging process. Every rating will be lost!"
>       , []
>       , \_ -> RestartCmd
>       )
>     , ( "correct"
>       , "Rates the result of the current expression as correct."
>       , []
>       , \_ -> RateCmd Correct
>       )
>     , ( "wrong"
>       , "Rates the result of the current expression as wrong."
>       , []
>       , \_ -> RateCmd Wrong
>       )
>     , ( "next"
>       , "Don't rate the current expression, with positive n the next "
>      ++ "n expressions."
>       , [ []
>         , [NaturalIntArgType]
>         ]
>       , \args -> case args of
>                      []         -> SkipCmd 1
>                      [IntArg n] -> SkipCmd n
>       )
>     , ( "undo"
>       , "Undo the previous command, with positive n the n commands "
>      ++ "executed before."
>       , [ []
>         , [NaturalIntArgType]
>         ]
>       , \args -> case args of
>                      []         -> UndoCmd 1
>                      [IntArg n] -> UndoCmd n
>       )
>     , ( "stack"
>       , "Show callstack."
>       , []
>       , \_ -> StackCmd
>       )
>     ]

> navigateCommandNames :: [String]
> navigateCommandNames = fst4 $ unzip4 navigateCommands

> inspectCommandNames :: [String]
> inspectCommandNames = fst4 $ unzip4 inspectCommands

> help :: Commands -> IO ()
> help = mapM_ (\(c, h, _, _) -> do
>                   putStr "\t"
>                   putStr c
>                   putStr "\t"
>                   putStrLn h
>              )

> navigateHelp :: IO ()
> navigateHelp = help navigateCommands

> inspectHelp :: IO ()
> inspectHelp = help inspectCommands

> fst4 :: (a, b, c, d) -> a
> fst4 (a, _, _, _) = a

> snd4 :: (a, b, c, d) -> b
> snd4 (_, b, _, _) = b

> thrd4 :: (a, b, c, d) -> c
> thrd4 (_, _, c, _) = c

> frth4 :: (a, b, c, d) -> d
> frth4 (_, _, _, d) = d

> unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
> unzip4 = foldr (\(a, b, c, d)~ (as, bs, cs, ds) ->
>                     (a : as, b : bs, c : cs, d : ds)
>                ) ([], [], [], [])

> lookup4 :: Eq a => a -> [(a, b, c, d)] -> Maybe (b, c, d)
> lookup4 _ [] = Nothing
> lookup4 kl ((k, v1, v2, v3) : kv4s)
>     | kl == k   = Just (v1, v2, v3)
>     | otherwise = lookup4 kl kv4s

Initializing and cleaning up
============================

    > start :: IO ()
    > start = SLE.initialise

    > stop :: IO ()
    > stop = SLE.restore

> start :: IO ()
> start = RL.initialize

> stop :: IO ()
> stop = RL.resetTerminal Nothing

Readline stuff
==============

> matchCommand :: String -> [String] -> [String]
> matchCommand c = filter (isPrefixOf c)

> completeWord :: [String] -> String -> Int -> Int -> IO (Maybe (String, [String]))
> completeWord commandNames word start end = do
>     return $ if start > 0 then 
>                  Nothing
>              else
>                  case matchCommand word commandNames of
>                      []  -> Nothing
>                      [c] -> Just (c, [])
>                      cs  -> Just (word, cs)

> completeNavigateWord :: String -> Int -> Int -> IO (Maybe (String, [String]))
> completeNavigateWord = completeWord navigateCommandNames

> completeInspectWord :: String -> Int -> Int -> IO (Maybe (String, [String]))
> completeInspectWord = completeWord inspectCommandNames

> setNavigateCompletion :: IO ()
> setNavigateCompletion = RL.setAttemptedCompletionFunction $ Just completeNavigateWord

> setInspectCompletion :: IO ()
> setInspectCompletion = RL.setAttemptedCompletionFunction $ Just completeInspectWord

Getting information from the user
=================================

> readLine = gnuReadLine

> simpleReadLine :: String -> IO String
> simpleReadLine prompt = do
>     putStr prompt
>     getLine

	> intermediateReadLine :: String -> IO String
	> intermediateReadLine prompt = do
	>     mi <- SLE.getLineEdited prompt
	>     case mi of
	>         Just i  -> return i
	>         Nothing -> return "quit"

> gnuReadLine :: String -> IO String
> gnuReadLine prompt = do
>     mi <- RL.readline prompt
>     case mi of
>         Just i  -> do
>             RL.addHistory i
>             return i
>         Nothing -> return "quit"

> parseArgs :: [[ArgType]] -> [String] -> Maybe [Arg]
> parseArgs [] [] = Just [] 
> parseArgs [] _  = Nothing
> parseArgs (ts : tss) as =
>     case parseArgs' ts as of
>         Just as' -> Just as'
>         Nothing  -> parseArgs tss as
>     where
>         parseArgs' [] [] = Just []
>         parseArgs' (_ : _) [] = Nothing
>         parseArgs' [] (_ : _) = Nothing
>         parseArgs' ((SpecialArgType s) : ts) (a : as)
>             | s == a = do
>                 pas <- parseArgs' ts as
>                 return $ (SpecialArg s) : pas
>             | otherwise = Nothing
>         parseArgs' (NaturalIntArgType : ts) (a : as) = 
>             case reads a :: [(Int, String)] of
>                 [(n, "")]
>                     | n > 0 -> do
>                         pas <- parseArgs' ts as
>                         return $ (IntArg n) : pas
>                     | otherwise ->
>                         Nothing

>                 _ -> Nothing

> getCmd :: Commands -> String -> IO Cmd
> getCmd commands p = do
>     i <- readLine p
>     let is = words i
>     case is of
>         []         -> getCmd commands p
>         (c : args) ->
>             case lookup4 c commands of
>                 Nothing -> do
>                     putStrLn "Invalid command."
>                     getCmd commands p
>                 Just (_, tss, gc) ->
>                     case parseArgs tss args of
>                         Nothing -> do
>                             putStrLn "Invalid parameters."
>                             getCmd commands p
>                         Just pagrs -> return $ gc pagrs

> getNavigateCmd :: String -> IO Cmd
> getNavigateCmd p = do
>     setNavigateCompletion
>     getCmd navigateCommands $ p ++ " > "

> getInspectCmd :: String -> IO Cmd
> getInspectCmd p = do
>     setInspectCompletion
>     getCmd inspectCommands $ p ++ " $ "

Showing information to the user
===============================

> putLabledInfos :: [(String, String)] -> IO ()
> putLabledInfos =
>     mapM_ ( \(label, info) -> do
>                 putStr label
>                 putStr ": "
>                 putStrLn info
>           )

> putEndMsg :: [(String, String)] -> IO ()
> putEndMsg is = do
>     putStrLn "\nReached end of computation...\n"
>     putLabledInfos is
>     putStrLn "\nRestarting computation...\n"

