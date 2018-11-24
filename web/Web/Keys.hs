{-# LANGUAGE TypeApplications #-}

module Web.Keys where

import Data.ByteString (ByteString)
import Data.Vault.Lazy
import Control.Monad.IO.Class
import Network.HTTP.Client (Manager)
import Network.Wai (vault)
import Network.Wai.Session (Session)
import Prelude hiding (lookup)
import System.IO.Unsafe

import qualified IPC

{-# NOINLINE managerKey #-}
managerKey :: Key Manager
managerKey = unsafePerformIO newKey

getManager = lookup managerKey . vault

{-# NOINLINE broadcastKey #-}
broadcastKey :: Key (IPC.Msg -> IO ())
broadcastKey = unsafePerformIO newKey

broadcast req msg =
    let Just bc = lookup broadcastKey (vault req)
     in liftIO $ bc msg

{-# NOINLINE sessionKey #-}
sessionKey :: Key (Session IO ByteString ByteString)
sessionKey = unsafePerformIO newKey
