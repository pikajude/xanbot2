{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad

import qualified IPC
import Print
import qualified Twitch
import qualified Web

main :: IO ()
main = do
    ch <- newChan @IPC.Msg
    void $
        waitAnyCancel =<< mapM async [Twitch.bot (readChan ch), Web.server (broadcast ch)]
  where
    broadcast ch msg = do
        $info ("Broadcasting: " ++ show msg)
        writeChan ch msg
