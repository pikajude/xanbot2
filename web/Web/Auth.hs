{-# Language ScopedTypeVariables #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-# Language DataKinds #-}

module Web.Auth where

import           Data.Text.Encoding
import           Data.Aeson
import           Servant.Foreign
import           Servant.JS
import           Data.Text                      ( Text )
import           Network.Wai                    ( Request
                                                , requestHeaders
                                                )
import           Servant
import           Servant.API
import           Servant.Server
import           Servant.Server.Experimental.Auth
import           Web.Cookie
import           Web.JWT                 hiding ( JSON )

jwtHandler :: AuthHandler Request (JWT VerifiedJWT)
jwtHandler = mkAuthHandler $ \req -> either throw401 pure $ do
    c <- maybeToEither "Missing cookies" $ lookup "cookie" $ requestHeaders req
    session <- maybeToEither "Missing cookie" $ lookup "_SESSION" $ parseCookies
        c
    maybeToEither "Invalid JWT"
        $ decodeAndVerifySignature (secret "secret") (decodeUtf8 session)
    where
        maybeToEither s = maybe (Left s) Right
        throw401 m = throwError $ err401
            { errBody = encode $ object ["message" .= (m :: Text)]
            }

type instance AuthServerData (AuthProtect "jwt") = JWT VerifiedJWT

getUser :: JWT VerifiedJWT -> Text
getUser = stringOrURIToText . (\(Just x) -> x) . sub . claims
