{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitch where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.HashMap.Strict as H
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Network.IRC.Base
import Network.IRC.Parser
import qualified Network.Simple.TCP.TLS as TLS
import System.Exit

import qualified IPC
import Print
import qualified Store
import qualified Twitch.Command as Command
import Twitch.Env (runTwitchEnv)
import Twitch.Env.TH (send)

bot :: IO IPC.Msg -> IO ()
bot receive = do
    tok <- B.readFile "token.secret"
    cs <- TLS.getDefaultClientSettings ("irc.chat.twitch.tv", ":6697")
    $info "Connecting to twitch IRC"
    TLS.connect cs "irc.chat.twitch.tv" "6697" $ \(sock, addr) -> do
        sc <- sockChan sock
        forkIO $
            forever $
            runTwitchEnv sc $ do
                m <- liftIO receive
                case m of
                    IPC.NewUser t -> $send $ mconcat ["JOIN #", encodeUtf8 t]
                    _ -> return ()
        runTwitchEnv sc $ do
            $info [i|Connected to #{addr}|]
            $send (mconcat ["PASS ", tok])
            $send "NICK xan666bot"
            fix
                (\f res ->
                     case res of
                         Partial y -> do
                             msg <- liftIO $ TLS.recv sock
                             case msg of
                                 Nothing -> $err "Lost connection to Twitch!"
                                 Just msg' -> f (y msg')
                         Fail {} -> $err (show res)
                         Done rest m -> do
                             respond m
                             f (parse message_ rest))
                (parse message_ "")

sockChan :: TLS.Context -> IO (Chan U.ByteString)
sockChan sock = do
    ch <- newChan
    forkIO $
        forever $ do
            c <- readChan ch
            TLS.send sock c
    return ch

message_ =
    Message <$> optionMaybe (prefix <* spaces) <*> command <*> many (spaces *> parameter) <*
    crlf <?> "message"
  where
    optionMaybe p = option Nothing (Just <$> p)

respond m@Message {..} = do
    $info ("> " ++ U.toString (encode m))
    case msg_command of
        "376" -> do
            Store.Store {users} <- Store.get
            forM_ (H.keys users) $ \chan -> $send (mconcat ["JOIN #", encodeUtf8 chan])
        "PING" -> $send "PONG :tmi.twitch.tv"
        "PRIVMSG"
            | [chan, words] <- msg_params
            , Just (NickName n _ _) <- msg_prefix ->
                Command.respond (decodeUtf8 n, decodeUtf8 chan, decodeUtf8 words)
        _ -> return ()
