{-# LANGUAGE FlexibleContexts #-}
{- | The monads MsgMonad and MsgMonadIO provide a common way to log warning
     messages and to stop execution when an error occurs. They may be used to
     integrate different compiler passes smoothly.

     (c) 2009, Holger Siegel.
-}

module Curry.Base.MessageMonad where

import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Identity

import Curry.Base.Position

{- | Message monad transformer enabling the reporting of 'WarnMsg's as
     warnings and additionally a 'WarnMsg' as an error message.
-}
type MsgMonadT m = ErrorT WarnMsg (WriterT [WarnMsg] m)

-- | Simple message monad
type MsgMonad = MsgMonadT Identity

-- | Message monad with underlying 'IO' monad
type MsgMonadIO = MsgMonadT IO

-- | Data type for warning messages
data WarnMsg = WarnMsg
  { warnPos :: Maybe Position -- ^ optional source code position
  , warnTxt :: String         -- ^ the message itself
  }

instance Error WarnMsg where
  noMsg  = WarnMsg Nothing "Failure!"
  strMsg = WarnMsg Nothing

instance Show WarnMsg where
  show = showWarning

-- | Show a 'WarnMsg' as a warning
showWarning :: WarnMsg -> String
showWarning w = "Warning: " ++ pos ++ warnTxt w
  where pos = case warnPos w of
                Nothing -> ""
                Just p  -> show p ++ ": "

-- | Show a 'WarnMsg' as an error
showError :: WarnMsg -> String
showError w = "Error: " ++ pos ++ warnTxt w
  where pos = case warnPos w of
                Nothing -> ""
                Just p -> show p ++ ": "

-- | Evaluate the value of a 'MsgMonad a'
runMsg :: MsgMonad a -> (Either WarnMsg a, [WarnMsg])
runMsg = runIdentity . runWriterT . runErrorT

{- | Directly evaluate to the success value of a 'MsgMonad a'. Errors are
     converted in a call to the 'error' function.
-}
ok :: MsgMonad a -> a
ok = either (error . showError) id . fst . runMsg

-- | Sequence 'MsgMonad' action inside the 'IO' monad.
runMsgIO :: MsgMonad a -> (a -> IO (MsgMonad b)) -> IO (MsgMonad b)
runMsgIO m f = case runMsg m of
  (Left  e, msgs) -> return (tell msgs >> throwError e)
  (Right x, msgs) -> do
    m' <- f x
    case runMsg m' of
      (Left _  , _    ) -> return m'
      (Right x', msgs') -> return (tell (msgs ++ msgs') >> return x')

-- | Convert a 'MsgMonad' to a 'MsgMonadIO'
dropIO :: MsgMonad a -> MsgMonadIO a
dropIO m = case runMsg m of
  (Left  e, msgs) -> tell msgs >> throwError e
  (Right x, msgs) -> tell msgs >> return x

-- | Abort the computation with an error message
failWith :: (MonadError a m, Error a) => String -> m b
failWith = throwError . strMsg

-- | Abort the computation with an error message at a certain position
failWithAt :: (MonadError WarnMsg m) => Position -> String -> m a
failWithAt p = throwError . WarnMsg (Just p)

-- | Report a warning message
warnMessage :: (MonadWriter [WarnMsg] m) => String -> m ()
warnMessage s = tell [WarnMsg Nothing s]

-- | Report a warning message for a given position
warnMessageAt :: (MonadWriter [WarnMsg] m) => Position -> String -> m ()
warnMessageAt p s  = tell [WarnMsg (Just p) s]
