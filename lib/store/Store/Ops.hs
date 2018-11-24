module Store.Ops where

import Control.Monad.IO.Class
import Data.Aeson.TH
import qualified Data.HashMap.Strict as H
import Data.Text
import Lens.Micro.Platform

import qualified Store

data AccessToken = AccessToken
    { access_token :: Text
    , refresh_token :: Text
    , scope :: [Text]
    } deriving (Show)

deriveFromJSON defaultOptions ''AccessToken

addUser acc who =
    liftIO $ Store.with $ \st ->
        if H.member (Store.name who) (Store.users st)
            then pure (st, True)
            else pure (_add st, False)
  where
    _add st =
        st & Store.usersL . at (Store.name who) ?~
        Store.UserStore (access_token acc) (refresh_token acc) who mempty mempty
