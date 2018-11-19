{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Web where

import qualified Data.Aeson as A
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Internal (Response(..))
import Print
import qualified Store

server = do
    $info "Starting webserver on 0.0.0.0:8080"
    run 8080 $
        logMiddleware $ \req resp ->
            case pathInfo req of
                [] -> do
                    cfg <- Store.get
                    resp $ responseLBS status200 [] (A.encode cfg)
                _ -> resp $ responseLBS status404 [] "not found"

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
