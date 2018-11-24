{-# OPTIONS_GHC -ddump-splices #-}

module Web.Path where

import Web.Routes.TH

data Route
    = Home
    | Callback
    deriving (Show)

derivePathInfo ''Route
