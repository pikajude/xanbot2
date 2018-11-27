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

import           Data.FileEmbed
import           Data.Maybe
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Network.HTTP.Types      hiding ( Header )
import           Network.Wai
import           Network.Wai.Application.Static ( staticApp )
import           Network.Wai.Handler.Warp
import           Network.Wai.Internal           ( Response(..) )
import           Servant                        ( throwError
                                                , serveDirectoryFileServer
                                                )
import           System.Environment
import           Text.Read
import           WaiAppStatic.Storage.Embedded
import           Web.Cookie                     ( parseCookies )
import           Web.JWT                 hiding ( JSON )

import           Servant.API
import           Servant.Server
import           Servant.Server.Experimental.Auth

import qualified IPC
import           Print
import qualified Web.Auth                      as Auth
import qualified Web.API.Callback              as Callback
import qualified Web.API.Commands              as Commands
import qualified Web.Static                    as Static

type API = Callback.Endpoint
      :<|> Commands.Endpoint

type All = "api" :> API :<|> "static" :> Raw :<|> Raw

apiServer :: Server All
apiServer =
    (Callback.endpoint :<|> Commands.endpoint)
        :<|> serveDirectoryFileServer "static"
        :<|> Tagged
                 (\_ resp -> resp $ responseLBS
                     status200
                     [("Content-Type", "text/html;charset=utf8")]
                     indexFile
                 )
    where indexFile = $(embedStringFile "html/index.html")

server :: (IPC.Msg -> IO ()) -> IO ()
server _broadcast = do
    port <- fromMaybe 8080 . (>>= readMaybe) <$> lookupEnv "PORT"
    $info [i|Starting webserver on 0.0.0.0:#{port}|]
    run port $ logMiddleware $ serveWithContext @All
        Proxy
        (Auth.jwtHandler :. EmptyContext)
        apiServer

logMiddleware :: Middleware
logMiddleware f req resp = f
    req
    (\resp' -> do
        let status = case resp' of
                ResponseFile st _ _ _  -> st
                ResponseBuilder st _ _ -> st
                ResponseStream  st _ _ -> st
                ResponseRaw{}          -> ok200
        $info [i|#{requestMethod req} #{rawPathInfo req} #{statusCode status}|]
        resp resp'
    )
