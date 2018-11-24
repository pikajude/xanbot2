module Twitch.Env.TH where

import qualified Data.ByteString.UTF8 as U

import Print (info)
import Twitch.Env

send = do
    inf <- info
    [|(\m -> do
           $(pure inf) ("< " ++ U.toString m)
           s <- ask
           liftIO $ writeChan s (m <> "\r\n")) :: U.ByteString -> Env ()|]
