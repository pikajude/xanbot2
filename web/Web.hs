{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Web where

import Control.Monad.Reader
import Control.Monad.Trans.Except
import qualified Data.Aeson as A
import Data.ByteString.Builder (toLazyByteString)
import Data.Default (def)
import Data.Maybe
import Data.String
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vault.Lazy as V
import Lens.Micro.Platform
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.HTTP.Types.URI
import Network.URI
import Network.Wai
import Network.Wai.Application.Static (staticApp)
import Network.Wai.Handler.Warp
import Network.Wai.Internal (Response(..))
import Network.Wai.Session
import Network.Wai.Session.ClientSession
import System.Environment
import Text.Blaze.Html.Renderer.Utf8
import Text.Hamlet
import Text.Read
import WaiAppStatic.Storage.Embedded
import qualified Web.ClientSession as Session

import qualified IPC
import Print
import Print.HTTPDebug
import qualified Store
import qualified Web.Keys as Keys
import Web.OAuth
import qualified Web.Static as Static

import qualified Web.Page.Callback as Callback
import qualified Web.Page.Home as Home

server :: (IPC.Msg -> IO ()) -> IO ()
server broadcast = do
    port <- fromMaybe 8080 . (>>= readMaybe) <$> lookupEnv "PORT"
    $info [i|Starting webserver on 0.0.0.0:#{port}|]
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
