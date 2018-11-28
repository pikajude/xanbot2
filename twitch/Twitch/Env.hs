{-# LANGUAGE PartialTypeSignatures #-}

module Twitch.Env where

import           Control.Concurrent.Chan
import           Control.Monad.Reader
import qualified Data.ByteString.UTF8          as U

type Env = ReaderT (Chan U.ByteString) IO

runTwitchEnv :: _ -> Env a -> IO a
runTwitchEnv = flip runReaderT
