{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Server.Message where

import           Data.MessagePack
import           Data.Text
import           GHC.Generics
import Control.Lens.TH

data ServerMessage = HANDSHAKE deriving (Show, Generic)

data ClientMessage = Auth Text deriving (Show, Generic)

instance MessagePack ClientMessage

instance MessagePack ServerMessage

makePrisms ''ServerMessage

makePrisms ''ClientMessage
