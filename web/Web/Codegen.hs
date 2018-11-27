{-# Language StandaloneDeriving #-}
{-# Language QuasiQuotes #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Web.Codegen where

import           Data.Text                      ( Text
                                                , pack
                                                , intercalate
                                                )
import           Data.Aeson
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH
import           NeatInterpolation
import           Servant
import           Data.Aeson.Flow
import           Servant.Foreign
import           Web.Codegen.Instances

content proxy = listFromAPI @Flow @Text Proxy Proxy proxy

gencode name = do
    decs <- deriveJSON defaultOptions name
    inst <-
        [d|instance HasForeignType Flow Text $(conT name) where
               typeFor lang typ p = showFlowType $ defaultFlowType defaultOptions p|]
    return $ decs ++ inst
