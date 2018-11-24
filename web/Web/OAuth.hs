{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Web.OAuth where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as U
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Types

import Store (UserInfo)
import Store.Ops (AccessToken(..))

getAuthCode man code = do
    body <-
        liftIO $ do
            authUrl <- parseRequest "https://id.twitch.tv/oauth2/token"
            httpLbs
                (setQueryString
                     [ ("client_id", Just "z92ktp9wj39vy8wc0t8q2k52q41pb3")
                     , ("client_secret", Just "gszkd4zb7rabj1pto6cut0l7khshhm")
                     , ("grant_type", Just "authorization_code")
                     , ("code", Just code)
                     , ("redirect_uri", Just CALLBACK)
                     ]
                     (authUrl {method = "POST"}))
                man
    ExceptT $ return $ tryDecode @AccessToken body

whoami man AccessToken {..} = do
    whoamiUrl <- liftIO $ parseRequest "https://api.twitch.tv/kraken/user"
    body <-
        liftIO $
        httpLbs
            (whoamiUrl
                 { requestHeaders =
                       [ ("Authorization", "OAuth " <> encodeUtf8 access_token)
                       , ("Accept", "application/vnd.twitchtv.v5+json")
                       ]
                 })
            man
    ExceptT $ return $ tryDecode @UserInfo body

tryDecode :: FromJSON a => Response ByteString -> Either String a
tryDecode resp
    | statusCode (responseStatus resp) >= 400 = Left $ U.toString $ responseBody resp
    | otherwise =
        case decode (responseBody resp) of
            Just c -> Right c
            Nothing -> Left "success response, but couldn't decode"
