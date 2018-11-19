module Secret where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Base64.Lazy as B64
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)

getClientAuth :: IO (ByteString, ByteString, Text)
getClientAuth = do
    r <- B.readFile "client.secret"
    return $ Binary.decode $ either error id $ B64.decode r

setClientAuth :: (ByteString, ByteString, Text) -> IO ()
setClientAuth a = B.writeFile "client.secret" $ B64.encode $ Binary.encode a
