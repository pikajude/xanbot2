{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}

module Web.API.Callback where

import           Data.Time.Clock.POSIX
import           Control.Monad.Except
import           Data.Aeson              hiding ( Success )
import           Data.Aeson.TH
import           Data.ByteString.Builder
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding       as LT
import qualified Data.Text.Lazy                as LT
import           Network.HTTP.Client.TLS
import           Servant                        ( err401
                                                , errBody
                                                , throwError
                                                )
import           Servant.API
import           Web.Cookie                     ( renderCookiesText )
import           Web.JWT                 hiding ( JSON )
import           Web.OAuth

import           Print.HTTPDebug
import qualified Store
import qualified Store.Ops                     as Ops

data AuthCode = AuthCode
    { code :: Text
    }

deriveJSON defaultOptions ''AuthCode

data AuthResponse = Success | UserExists | Failure String deriving Show

deriveJSON defaultOptions ''AuthResponse

endpoint (AuthCode c) = do
    man  <- newTlsManagerWith (debugManager tlsManagerSettings)
    who' <- runExceptT $ do
        acc <- getAuthCode man (encodeUtf8 c)
        w   <- whoami man acc
        return (acc, w)
    case who' of
        Right (acc, who) -> do
            exists <- Ops.addUser acc who
            pt     <- liftIO getPOSIXTime
            return $ addHeader (payload who pt)
                               (if exists then UserExists else Success)
        Left x -> throwError (err401 { errBody = encode $ Failure x })
    where
        payload who pt = LT.decodeUtf8 $ toLazyByteString $ renderCookiesText
            [ ( "_SESSION"
              , encodeSigned
                      HS256
                      _secret
                      (def
                          { sub = stringOrURI $ Store.display_name who
                          , iat = numericDate pt
                          , unregisteredClaims = [ ( "logo"
                                                   , String (Store.logo who)
                                                   )
                                                 ]
                          }
                      )
                  <> "; Path=/"
              )
            ]

_secret = secret "secret"

type Endpoint = "authorize"
             :> ReqBody '[JSON] AuthCode
             :> Post '[JSON] (Headers '[Header "Set-Cookie" LT.Text] AuthResponse)
