{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Page.Home where

import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text.Lazy.Encoding as LT
import Lens.Micro.Platform
import Network.HTTP.Types.URI
import Text.Blaze.Html.Renderer.Utf8
import Text.Hamlet

import qualified Store
import Web.Page.Prelude
import Web.Session (K(..))
import qualified Web.Session as Session

page = do
    user <- Session.getUser
    layout $ do
        setTitle "Home"
        pure [shamlet|<div #like_button>|]
  where
    uri =
        toLazyByteString $
        "https://id.twitch.tv" <>
        encodePath
            ["oauth2", "authorize"]
            [ ("client_id", Just "z92ktp9wj39vy8wc0t8q2k52q41pb3")
            , ("redirect_uri", Just CALLBACK)
            , ("response_type", Just "code")
            , ("scope", Just "user_read channel_editor")
            ]
