module Store.Lens where

import Data.Aeson.TH
import Data.Char
import Language.Haskell.TH
import Lens.Micro.Platform

makeLenses =
    makeLensesWith
        (lensRules & lensField .~ (\_ _ n -> [TopName (mkName (nameBase n ++ "L"))]))

snakeCase :: String -> String
snakeCase = u . applyFirst toLower
  where
    u [] = []
    u (x:xs)
        | isUpper x = '_' : toLower x : snakeCase xs
        | otherwise = x : u xs

applyFirst :: (Char -> Char) -> String -> String
applyFirst _ [] = []
applyFirst f [x] = [f x]
applyFirst f (x:xs) = f x : xs

opts = defaultOptions {fieldLabelModifier = snakeCase}
