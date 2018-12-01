{-# Language FlexibleContexts #-}
{-# Language ConstraintKinds #-}
{-# Language DuplicateRecordFields #-}

module IPC where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Text                      ( Text )
import           Control.Concurrent             ( forkIO )
import           System.IO.Unsafe
import           Control.Monad
import           Control.Concurrent.STM.TChan
import           Data.Monoid                    ( First )
import           Control.Concurrent.STM

newtype Chan = Chan { unChan :: TChan Msg }

type MonadIPC m = (MonadIO m, MonadReader Chan m)

withIPC f = do
    _notifyChan <- newBroadcastTChanIO
    runReaderT f (Chan _notifyChan)

{-# NOINLINE _notifyChan #-}
_notifyChan :: TChan Msg
_notifyChan = unsafePerformIO newBroadcastTChanIO

broadcast :: MonadIPC m => Msg -> m ()
broadcast m = do
    Chan t <- ask
    liftIO (atomically $ writeTChan t m)

subscribe :: MonadIPC m => m (IO Msg)
subscribe = do
    Chan t <- ask
    newt   <- liftIO $ atomically (dupTChan t)
    return $ atomically (readTChan newt)

subscribeTo :: MonadIPC m => Getting (First a) Msg a -> m (IO a)
subscribeTo p = do
    ch <- subscribe
    return $ fix $ \ f -> do
        x <- liftIO ch
        maybe f return (x ^? p)

data Msg
    = UserMsg UserMsg
    | CommandMsg CommandMsg
    | KeywordMsg KeywordMsg
    deriving (Show, Eq)

data UserMsg = NewUser
    { access_token :: Text
    , refresh_token :: Text
    } deriving (Show, Eq)

data CommandMsg = NewCommand { owner :: Text, match :: Text, command_text :: Text } deriving (Show, Eq)

data KeywordMsg = NewKeyword { owner :: Text, match :: Text, keyword_text :: Text } deriving (Show, Eq)

makePrisms ''Msg
