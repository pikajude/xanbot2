module Print.HTTPDebug where

import           Codec.Compression.GZip        as GZip
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString               as B
import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                )
import qualified Data.ByteString.UTF8          as U
import           Network.HTTP.Client.Internal
import           Print

debugManager :: ManagerSettings -> ManagerSettings
debugManager oldMgr = oldMgr
    { managerRawConnection = return $ \hs s i -> do
                                 connector <- managerRawConnection oldMgr
                                 conn      <- connector hs s i
                                 return $ debug_ conn
    , managerTlsConnection = return $ \hs s i -> do
                                 connector <- managerTlsConnection oldMgr
                                 conn      <- connector hs s i
                                 return $ debug_ conn
    }
    where
        debug_ conn = conn
            { connectionRead  = do
                                    bytes <- connectionRead conn
                                    $dbg ("< " ++ maybeDecompress bytes)
                                    return bytes
            , connectionWrite = \bs -> do
                                    forM_ (lines $ U.toString bs)
                                          (\l -> $dbg ("> " ++ l))
                                    connectionWrite conn bs
            }

maybeDecompress x
    | "\x1F\x8B" `B.isPrefixOf` x = U.toString
        (toStrict $ GZip.decompress $ fromStrict x)
    | otherwise = U.toString $ B.takeWhile (liftA2 (&&) (/= 13) (/= 10)) x
