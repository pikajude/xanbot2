{-# Language TemplateHaskell #-}
module Web.Static.Hash where

import           Crypto.Hash.MD5
import           Data.Text.Encoding
import           Language.Haskell.TH
import           Data.Text                      ( pack )
import           Lens.Micro.Platform
import           System.Directory.Tree
import           Network.Mime
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Lazy          as B
import           Data.ByteString.UTF8           ( toString )

loadFile filename = do
    bytes <- B.readFile filename
    let etag = decodeUtf8 $ B16.encode $ hashlazy bytes
    return (etag, bytes)
