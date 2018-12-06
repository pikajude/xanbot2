{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# Language FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server where

import           Reflex
import           Network.WebSockets      hiding ( runServer )
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import qualified Data.Map                      as M
import           Server.Message
import           Control.Monad.Fix
import           Network.Socket
import           Data.FileEmbed
import           Control.Monad.IO.Class
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Web.Cookie
import           Control.Concurrent
import           Control.Lens
import           Data.Functor
import           Logger
import           Server.Types
import           Web.JWT                 hiding ( decode )
import           Data.Aeson

-- | A websocket server.
data Server t = Server {
    receivedMessage :: Event t (UniqueId, DataMessage),
    serverState :: Dynamic t ServerState
}

webSocketServer
  :: ( PerformEvent t m
     , MonadIO (Performable m)
     , MonadIO m
     , PostBuild t m
     , TriggerEvent t m
     , MonadHold t m
     , MonadSample t (Performable m)
     , MonadFix m
     )
  => Event t (UniqueId, Text)
  -> m (Server t)
webSocketServer sender = mdo
  sock     <- liftIO $ makeListenSocket "localhost" 8080
  pb       <- getPostBuild

  nextConn <- performEventAsync $ pb $> \fn ->
    void $ liftIO $ forkIO $ forever $ do
      (s, _) <- accept sock
      conn   <- makePendingConnection s defaultConnectionOptions
      authenticateOrClose fn conn

  (closeConn, doClose) <- newTriggerEvent

  allConnections <- fmap ServerState . incrementalToDynamic <$> holdIncremental
    mempty
    (  evToPatch nextConn
    <> ffor closeConn (\(y, _) -> PatchMap (M.singleton y Nothing))
    )

  newMsg <-
    performEventAsync
    $  leftmost [() <$ updated allConnections, () <$ newMsg]
    $> \fn -> do
         ServerState { clients } <- sample (current allConnections)
         case M.toList clients of
           [] -> return ()
           xs -> void $ liftIO $ forkIO $ do
             asyncs <- forM xs $ \(u, c) -> async $ catchJust
               isConnectionClosed
               ((,) u <$> receiveDataMessage (connection c))
               (\e -> doClose (u, e) >> noreturn)
             (_, msg) <- waitAnyCancel asyncs
             fn msg

  $logWithFormat formatUserMessage newMsg
  $logWithFormat formatServerMessage sender
  $logWithFormat formatUserErr closeConn

  performEvent_ $ ffor sender $ \(u, msg) -> do
    ServerState { clients } <- sample (current allConnections)
    case M.lookup u clients of
      Just y  -> liftIO $ sendTextData (connection y) msg
      Nothing -> return ()

  return Server { receivedMessage = newMsg, serverState = allConnections }
  where
    evToPatch = fmap (\(x, y) -> PatchMap (M.singleton x (Just y)))

    isConnectionClosed ConnectionClosed = Just ConnectionClosed
    isConnectionClosed c@CloseRequest{} = Just c
    isConnectionClosed _                = Nothing

    noreturn = liftIO $ forever (threadDelay 1000000)

authenticateOrClose
  :: ((UniqueId, Client) -> IO ()) -> PendingConnection -> IO ()
authenticateOrClose trigger pendingConn = do
  conn <- acceptRequest pendingConn
  u    <- newUniqueId
  let mjwt = do
        cookieText <- orelse "Missing cookies"
          $ lookup "Cookie" (requestHeaders $ pendingRequest pendingConn)
        sessionCookie <- orelse "Missing session cookie"
          $ lookup "_SESSION" (parseCookies cookieText)
        orelse "Invalid JWT"
          $ decodeAndVerifySignature _SECRET (decodeUtf8 sessionCookie)
  sendTextData conn . encode $ either NoLogin (const LoggedIn) mjwt
  trigger (u, Client { connection = conn, jwt = mjwt ^? _Right })
  where orelse s = maybe (Left s) Right

_SECRET :: Secret
_SECRET = binarySecret $(embedFile "session_key.bin")
