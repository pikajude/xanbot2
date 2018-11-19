module Main where

import Control.Concurrent.Async
import Control.Monad

import Print
import qualified Twitch
import qualified Web

main :: IO ()
main = void $ waitAnyCancel =<< mapM async [Twitch.bot, Web.server]
