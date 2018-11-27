{-# Language DataKinds #-}
{-# Language TypeOperators #-}

module Web.API.Commands where

import           Data.Text                      ( Text )
import           Servant.Server
import           Servant
import           Control.Monad.IO.Class
import           Web.Auth
import           Lens.Micro.Platform

import qualified Store

type Endpoint = "commands" :> AuthProtect "jwt" :> Get '[JSON] [Text]

endpoint :: Server Endpoint
endpoint jwt = liftIO
    $ Store.getList (Store.usersL . ix owner . Store.commandsL . traverse)
    where owner = getUser jwt
