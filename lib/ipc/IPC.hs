module IPC where

import           Data.Text                      ( Text )

data Msg =
    NewUser Text
    deriving (Show)
