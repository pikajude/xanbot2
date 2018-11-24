module Store.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Store.Lens

data Store = Store
    { users :: HashMap Text UserStore
    } deriving (Show, Eq)

data UserStore = UserStore
    { accessToken :: Text
    , refreshToken :: Text
    , userInfo :: UserInfo
    , commands :: HashMap Text Text
    , keywords :: HashMap Text Text
    } deriving (Show, Eq)

data UserInfo = UserInfo
    { name :: Text
    , _id :: Text
    , display_name :: Text
    , logo :: Text
    } deriving (Show, Eq)

deriveToJSON opts ''UserStore

deriveToJSON opts ''Store

deriveJSON opts ''UserInfo

instance FromJSON Store where
    parseJSON = withObject "Store" $ \o -> Store <$> o .:? "users" .!= mempty

instance FromJSON UserStore where
    parseJSON =
        withObject "UserStore" $ \o ->
            UserStore <$> o .: "access_token" <*> o .: "refresh_token" <*>
            o .: "user_info" <*>
            o .:? "commands" .!= mempty <*>
            o .:? "keywords" .!= mempty

makeLenses ''UserStore

makeLenses ''Store
