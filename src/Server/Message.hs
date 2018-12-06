{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Server.Message where

import           Data.Text
import           GHC.Generics
import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.Aeson.Flow

data ServerMessage = LoggedIn | NoLogin Text deriving (Show, Generic)

data ClientMessage = Register { code :: Text } | Login { jwtText :: Text } deriving (Show, Generic)

instance FlowTyped ServerMessage
instance FlowTyped ClientMessage

deriveJSON defaultOptions ''ServerMessage

deriveJSON defaultOptions ''ClientMessage

makePrisms ''ServerMessage

makePrisms ''ClientMessage

exportModule :: IO ()
exportModule = writeFlowModule
  defaultFlowModuleOptions
  "flow/generated/serverTypes.js"
  [Export @ServerMessage Proxy, Export @ClientMessage Proxy]
