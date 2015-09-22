> {-# OPTIONS_GHC -XFlexibleInstances #-}
> module Curry.Debugger.Tools.DeclarativeDebugger.CallStack ( CallStack
>                  , FuncCall (..)
>                  , new
>                  , top
>                  , push
>                  , pop
>                  )
>  where

> import Curry.Debugger.Tools.DeclarativeDebugger.Ratings ( Rating (..) )

> data CallStack = CS { stack :: [FuncCall] }
> data FuncCall = FC { mod  :: String
>                    , name :: String
>                    , args :: [String]
>                    , res :: Maybe String
>                    , rating :: Rating
>                    }

> instance Show CallStack where
>     showsPrec _prec (CS cs) = shows' 1 $ reverse cs where
>         shows' _n [] = showString "<Empty callstack>"
>         shows' n [c] = shows'' n c
>         shows' n (c : cs) = (shows' (n + 1) cs)
>                           . (showChar '\n')
>                           . (shows'' n c)
>         shows'' n c = (showChar '(')
>                     . (shows n)
>                     . (showString ") ")
>                     . (shows c)

> instance Show FuncCall where
>     showsPrec _prec (FC m f as re ra) = (case m of
>                                              "" ->
>                                                  id
>                                              _ ->
>                                                  (showString m)
>                                                . (showChar '.')
>                                         )
>                                       . (showString f)
>                                       . (foldl (\sf a -> sf
>                                                        . (showChar ' ')
>                                                        . (showString a)
>                                                ) id as)
>                                       . (case re of
>                                              Nothing ->
>                                                  id
>                                              Just r  -> 
>                                                  (showString " ==> ")
>                                                . (showString r)
>                                         )
>                                       . (showString " (")
>                                       . (shows ra)
>                                       . (showChar ')')
> new :: CallStack
> new = CS { stack = [] }

> top :: CallStack -> FuncCall
> top = head . stack

> push :: CallStack -> FuncCall -> CallStack
> push (CS cs) c = CS { stack = c : cs }

> pop :: CallStack -> CallStack
> pop (CS (c : cs)) = CS { stack = cs } 

