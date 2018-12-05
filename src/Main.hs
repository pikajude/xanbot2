{-# LANGUAGE GADTs #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}
{-# Language RecursiveDo #-}

module Main where

import           Data.Text                      ( pack )
import           Server
import           Reflex.Host.Basic
import           Data.Time.LocalTime
import           Reflex
import           Control.Monad.IO.Class
import           Server.Types

main :: IO ()
main = basicHostForever $ mdo
  greeting <- performEvent $ ffor recv $ \(u, _dm) -> do
    ti <- liftIO getZonedTime
    pure
      (u, pack ("Hi, user #" ++ show (intId u) ++ "! The time is " ++ show ti))

  Server { receivedMessage = recv } <- webSocketServer greeting

  pure ()
