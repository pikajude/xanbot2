{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}

module Store
    ( module Store
    , module Lens.Micro.Platform
    ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Thread.Delay
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)
import Data.Yaml
import Lens.Micro.Platform
import Print
import System.Directory
import System.Environment
import System.Exit
import System.IO.Unsafe

data Store = Store
    { channels :: HashMap Text ChannelStore
    } deriving (Show, Eq)

data ChannelStore = ChannelStore
    { commands :: HashMap Text Text
    , keywords :: HashMap Text Text
    } deriving (Show, Eq)

deriveJSON defaultOptions ''ChannelStore

deriveJSON defaultOptions ''Store

makeLensesFor
    [("commands", "commandsL"), ("keywords", "keywordsL"), ("owner", "ownerL")]
    ''ChannelStore

makeLensesFor [("channels", "channelsL")] ''Store

emptyStore :: Store
emptyStore = Store mempty

_FILE = fromMaybe "store.yaml" <$> lookupEnv "CONFIG"

{-# NOINLINE _store #-}
_store :: MVar Store
_store =
    unsafePerformIO $ do
        f <- _FILE
        forkIO $
            forever $ do
                delay (5 * 60 * 1000000)
                $info "Saving config to disk"
                encodeFile f =<< get
        ie <- doesFileExist f
        if ie
            then do
                df <- decodeFileEither f
                case df of
                    Left x -> $err [i|When loading config: #{x}|] >> newEmptyMVar
                    Right y -> $info [i|Loaded config from #{f}|] >> newMVar y
            else do
                $info "No config file found. Making a default one"
                encodeFile f emptyStore
                newMVar emptyStore

get = liftIO $ readMVar _store

update f = liftIO $ modifyMVar_ _store (return . f)

gets f = fmap (view f) get

get_ f = fmap (^? f) get
