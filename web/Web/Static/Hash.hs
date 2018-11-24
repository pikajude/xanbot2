module Web.Static.Hash where

import Crypto.Hash.MD5
import Data.Text.Encoding
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as B

loadFile filename = do
    bytes <- B.readFile filename
    let etag = decodeUtf8 $ B16.encode $ hashlazy bytes
    return (etag, bytes)
