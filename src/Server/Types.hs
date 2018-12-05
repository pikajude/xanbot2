module Server.Types
  ( UniqueId
  , newUniqueId
  , intId
  , ServerState(..)
  , Client(..)
  )
where

import           Web.JWT
import           Data.Unique
import           Data.Coerce
import           Data.Map                       ( Map )
import           Network.WebSockets

data Client = Client
    { jwt :: JWT VerifiedJWT
    , connection :: Connection
    }

newtype ServerState = ServerState { clients :: Map UniqueId Client }

newtype UniqueId = UniqueId Unique deriving (Eq, Ord)

instance Show UniqueId where
  show = show . hashUnique . coerce

newUniqueId :: IO UniqueId
newUniqueId = UniqueId <$> newUnique

intId :: UniqueId -> Int
intId (UniqueId u) = hashUnique u
