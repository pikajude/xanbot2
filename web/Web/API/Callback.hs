{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module Web.API.Callback where

import Control.Monad.Except
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Builder
import Data.String
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding as LT
import Network.HTTP.Client.TLS
import Servant (err401, errBody, throwError)
import Servant.API (addHeader)
import Web.Cookie (renderCookiesText)
import Web.JWT
import Web.OAuth

import Print.HTTPDebug
import qualified Store
import qualified Store.Ops as Ops

data AuthCode = AuthCode
    { code :: Text
    }

deriveJSON defaultOptions ''AuthCode

tryAuth (AuthCode c) = do
    man <- newTlsManagerWith (debugManager tlsManagerSettings)
    who' <-
        runExceptT $ do
            acc <- getAuthCode man (encodeUtf8 c)
            w <- whoami man acc
            return (acc, w)
    case who' of
        Right (acc, who) -> do
            exists <- Ops.addUser acc who
            let name = Store.display_name who
                logo = Store.logo who
            return $ addHeader (payload who) Nothing
        Left x -> throwError (err401 {errBody = encode x})
  where
    payload who =
        LT.decodeUtf8 $
        toLazyByteString $
        renderCookiesText
            [ ( "_SESSION"
              , encodeSigned
                    HS256
                    _secret
                    (def
                         { unregisteredClaims =
                               [ ("username", String (Store.display_name who))
                               , ("logo", String (Store.logo who))
                               ]
                         }))
            ]
    {-(\(AuthCode c) ->
         pure $
         addHeader
             (LT.decodeUtf8 $
              toLazyByteString $
              renderCookiesText
                  [ ( "_SESSION"
                    , encodeSigned
                          HS256
                          (secret "secret")
                          (def
                               { unregisteredClaims =
                                     [ ("username", A.String "xanax_ow")
                                     , ( "logo"
                                       , A.String
                                             "https://static-cdn.jtvnw.net/jtv_user_pictures/96db1389-fdcb-400c-85e7-cf7c501dd62c-profile_image-300x300.png")
                                     ]
                               }))
                  ])
             "success")-}

_secret = secret "secret"
