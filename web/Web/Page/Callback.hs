{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Page.Callback where

import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as H
import Lens.Micro.Platform
import Network.HTTP.Types.Status
import Network.Wai
import Text.Hamlet

import qualified IPC
import Print
import qualified Store
import qualified Store.Ops as Ops
import qualified Web.Keys as Keys
import Web.OAuth
import Web.Page.Prelude
import qualified Web.Session as Session
import Web.Session (K(..))

page = do
    req <- ask
    let Just man = Keys.getManager req
    case lookup "code" (queryString req) of
        Just (Just c) -> do
            who' <-
                runExceptT $ do
                    acc <- getAuthCode man c
                    w <- whoami man acc
                    return (acc, w)
            case who' of
                Right (acc, who) -> do
                    exists <- Ops.addUser acc who
                    when (not exists) $ Keys.broadcast req (IPC.NewUser $ Store.name who)
                    let name = Store.display_name who
                    Session.set req KUsername (Store.name who)
                    layout $ pure $(shamletFile "templates/callback.hamlet")
                Left e ->
                    layout $ do
                        setTitle "Error"
                        setStatus status500
                        pure [shamlet|Oh noey. Internal error: #{e}|]
        _ ->
            layout $ do
                setTitle "Code missing"
                setStatus status400
                pure [shamlet|No code given, try again|]
