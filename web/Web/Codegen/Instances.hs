{-# Language QuasiQuotes #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}
{-# Language MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Web.Codegen.Instances where

import           Data.Text
import           Servant
import           Servant.Foreign

data Flow

instance (HasForeignType lang ftype Text, HasForeign lang ftype api) =>
         HasForeign lang ftype (AuthProtect "jwt" :> api) where
    type Foreign ftype (AuthProtect "jwt" :> api) = Foreign ftype api
    foreignFor pl pf _ r =
        foreignFor
            pl
            pf
            (Proxy :: Proxy api)
            (r
                 { _reqHeaders =
                       _reqHeaders r <>
                       [ HeaderArg
                             (Arg
                                  { _argName = PathSegment "Cookie"
                                  , _argType =
                                        typeFor
                                            @lang
                                            @ftype
                                            @Text
                                            Proxy
                                            Proxy
                                            Proxy
                                  })
                       ]
                 })

instance HasForeignType Flow Text api => HasForeignType Flow Text (Headers hs api) where
    typeFor lang typ _ = typeFor lang typ (Proxy :: Proxy api)

instance HasForeignType Flow Text Text where
    typeFor _ _ _ = "string"

instance HasForeignType Flow Text a => HasForeignType Flow Text [a] where
    typeFor lang typ _ = mconcat ["Array<", typeFor lang typ (Proxy :: Proxy a), ">"]
