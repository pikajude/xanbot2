{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Twitch.Command where

import           Control.Concurrent.Chan
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.ByteString.UTF8          as U
import qualified Data.HashMap.Strict           as H
import qualified Data.Text                     as T
import           Data.Text.Encoding
import qualified Network.Simple.TCP.TLS        as TLS

import           Print
import qualified Store
import           Store                          ( UserStore(..) )
import           Twitch.Env.TH

respond (user, channel, phrase) = do
    st <- Store.get_ (Store.usersL . Store.ix channel)
    forM_ st $ \c -> do
        forM_ (H.filterWithKey (\kw _ -> hasKeyword phrase kw) (keywords c))
            $ \keyword_response -> $send
                  (mconcat
                      [ "PRIVMSG "
                      , encodeUtf8 channel
                      , " :"
                      , encodeUtf8 keyword_response
                      ]
                  )

hasKeyword phrase kw = kw `elem` T.words phrase
