{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Web.Session where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString
import Data.Serialize (Serialize)
import Data.Text
import Data.Text.Encoding
import qualified Data.Vault.Lazy as V
import Network.Wai (vault)
import Prelude hiding (lookup)

import qualified Store
import qualified Web.Keys as Keys

getUser = do
    req <- ask
    do cu <- get req KUsername
       case cu of
           Just r -> Store.get_ (Store.usersL . Store.ix r . Store.userInfoL)
           Nothing -> pure Nothing

get req k =
    let Just (lookup, _) = V.lookup Keys.sessionKey (vault req)
     in liftIO $ fmap (>>= deserialize k) $ lookup (keyName k)

set req k v =
    let Just (_, setter) = V.lookup Keys.sessionKey (vault req)
     in liftIO $ setter (keyName k) (serialize k v)

data K :: * -> * where
    KUsername :: K Text

class SessionKey k where
    type Output k :: *
    keyName :: k -> ByteString
    deserialize :: k -> ByteString -> Maybe (Output k)
    serialize :: k -> Output k -> ByteString

instance SessionKey (K a) where
    type Output (K a) = a
    keyName KUsername = "username"
    deserialize KUsername b = Just $ decodeUtf8 b
    serialize KUsername b = encodeUtf8 b
