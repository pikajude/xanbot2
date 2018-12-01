{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DeriveGeneric #-}

module Store
    ( module Store
    , module Store.Types
    , module Control.Lens
    )
where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Concurrent.Thread.Delay
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Aeson.TH
import           Data.ByteString                ( ByteString )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as H
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Yaml
import           Control.Lens
import           Print
import           Store.Types
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO.Unsafe

_FILE = fromMaybe "store.yaml" <$> lookupEnv "CONFIG"

emptyStore = Store mempty

{-# NOINLINE _store #-}
_store :: MVar Store
_store = unsafePerformIO $ do
    f  <- _FILE
    ie <- doesFileExist f
    if ie
        then do
            df <- decodeFileEither f
            case df of
                Left  x -> $err [i|When loading config: #{x}|] >> exitFailure
                Right y -> do
                    $info [i|Loaded config from #{f}|]
                    $dbg (show y)
                    newMVar y
        else do
            $info "No config file found. Making a default one"
            newMVar emptyStore

get = liftIO $ readMVar _store

update f = liftIO $ modifyMVar_
    _store
    (\g -> do
        let x = f g
        flip encodeFile x =<< _FILE
        return x
    )

with f = liftIO $ modifyMVar
    _store
    (\g -> do
        (a, b) <- f g
        flip encodeFile a =<< _FILE
        return (a, b)
    )

gets f = fmap (view f) get

get_ f = fmap (^? f) get

getList f = fmap (^.. f) get
