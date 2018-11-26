{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Web where

import Control.Monad.Reader
import Control.Monad.Trans.Except
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.ByteString (isPrefixOf)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Default (def)
import Data.FileEmbed
import Data.Maybe
import Data.Proxy (Proxy(Proxy))
import Data.String
import Data.Text (Text, pack)
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vault.Lazy as V
import Lens.Micro.Platform
import Network.HTTP.Client.TLS
import Network.HTTP.Types hiding (Header)
import Network.HTTP.Types.URI
import Network.URI
import Network.Wai
import Network.Wai.Application.Static (staticApp)
import Network.Wai.Handler.Warp
import Network.Wai.Internal (Response(..))
import Network.Wai.Session
import Network.Wai.Session.ClientSession
import Servant (throwError)
import Servant.Utils.StaticFiles
import System.Environment
import Text.Blaze.Html.Renderer.Utf8
import Text.Hamlet
import Text.Read
import WaiAppStatic.Storage.Embedded
import qualified Web.ClientSession as Session
import Web.Cookie (parseCookies, renderCookiesText)
import Web.JWT
    ( Algorithm(HS256)
    , JWT
    , VerifiedJWT
    , decodeAndVerifySignature
    , encodeSigned
    , secret
    , unregisteredClaims
    )

import Servant.API
import Servant.API.Experimental.Auth
import Servant.Server
import Servant.Server.Experimental.Auth

import qualified IPC
import Print
import Print.HTTPDebug
import qualified Store
import Web.API.Callback
import qualified Web.Keys as Keys
import Web.OAuth
import qualified Web.Static as Static

import qualified Web.Page.Callback as Callback
import qualified Web.Page.Home as Home

type API
     = "authorize" :> ReqBody '[ JSON] AuthCode :> Post '[ JSON] (Headers '[ Header "Set-Cookie" LT.Text] (Maybe Text)) :<|> AuthProtect "jwt" :> "whoami" :> Get '[ JSON] Text :<|> Raw

apiServer :: Server API
apiServer = tryAuth :<|> pure . pack . show :<|> Tagged serveStatic

jwtHandler :: AuthHandler Request (JWT VerifiedJWT)
jwtHandler =
    mkAuthHandler $ \req ->
        either throw401 pure $ do
            c <- maybeToEither "Missing cookies" $ lookup "cookie" $ requestHeaders req
            session <- maybeToEither "Missing cookie" $ lookup "_SESSION" $ parseCookies c
            maybeToEither "Invalid JWT" $
                decodeAndVerifySignature (secret "secret") (decodeUtf8 session)
  where
    maybeToEither s = maybe (Left s) Right
    throw401 msg = throwError $ err401 {errBody = msg}

type instance AuthServerData (AuthProtect "jwt") = JWT VerifiedJWT

serveStatic req resp
    | Just acc <- lookup "Accept" (requestHeaders req)
    , "text/html" `isPrefixOf` acc =
        resp $
        responseLBS
            status200
            [("Content-Type", "text/html;charset=utf8")]
            $(embedStringFile "html/index.html")
    | otherwise = staticApp $(mkSettings Static.embedded) req resp

server :: (IPC.Msg -> IO ()) -> IO ()
server broadcast = do
    port <- fromMaybe 8080 . (>>= readMaybe) <$> lookupEnv "PORT"
    $info [i|Starting webserver on 0.0.0.0:#{port}|]
    run port $
        logMiddleware $ serveWithContext @API Proxy (jwtHandler :. EmptyContext) apiServer
        {-
    man <- newTlsManagerWith (debugManager tlsManagerSettings)
    sessionKey <- Session.getDefaultKey
    run port $
        withSession (clientsessionStore sessionKey) "_SESSION" def Keys.sessionKey $
        logMiddleware $ \req' resp ->
            let req =
                    req'
                        { vault =
                              V.insert Keys.broadcastKey broadcast $
                              V.insert Keys.managerKey man (vault req')
                        }
                handler =
                    case pathInfo req of
                        [] -> Left Home.page
                        ["callback"] -> Left Callback.page
                        _ -> Right $ staticApp $(mkSettings Static.embedded)
             in case handler of
                    Left f -> resp =<< runReaderT f req
                    Right e -> e req resp
                    -}

logMiddleware :: Middleware
logMiddleware f req resp =
    f req
        (\resp' -> do
             let status =
                     case resp' of
                         ResponseFile st _ _ _ -> st
                         ResponseBuilder st _ _ -> st
                         ResponseStream st _ _ -> st
             $info [i|#{requestMethod req} #{rawPathInfo req} #{statusCode status}|]
             resp resp')
