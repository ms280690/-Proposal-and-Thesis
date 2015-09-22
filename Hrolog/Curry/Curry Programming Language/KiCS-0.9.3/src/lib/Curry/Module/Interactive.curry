module Interactive where

interactiveSols :: [_] -> IO ()
interactiveSols [] = putStrLn "No more Solutions"
interactiveSols (x:xs) = do
  printTerm x 
  putStrLn "More?"
  line <- getLine
  case line of
   'n':_ -> return ()
   'N':_ -> return ()
   _ -> interactiveSols xs

printIO :: IO a -> IO ()
printIO act = act >>= \x -> putStr "IO: " >> (printTerm $!! x)

printTerm :: _ -> IO ()
printTerm external

{-
--showTreeToDepth :: Int -> Int -> SearchTree a -> String 
showTreeToDepth _ _ _ Fail = showString "Fail"
showTreeToDepth _ _ _ Suspend = showString "Suspend"
showTreeToDepth _ _ d (Value x1) = showParen (d>10) showStr
   where
    showStr  = showString "Value " . showsPrec 11 x1
showTreeToDepth maxDepth curDepth d (Or xs) = showParen (d>10) showStr
  where
    showStr | maxDepth==curDepth = showString "Or [...]"
            | otherwise          = 
      showString "Or [" . 
      foldr1 (showString "," . (.)) 
             (map (showTreeToDepth maxDepth (curDepth+1) 0) xs) .
      showString "]"

showString s = 


-}

