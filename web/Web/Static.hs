module Web.Static where

import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed
import Data.Text (pack)
import WaiAppStatic.Storage.Embedded
import Web.Static.Hash (loadFile)

embedded =
    mapM
        file
        [ ("css/foundation.min.css", "text/css")
        , ("css/main.css", "text/css")
        , ("js/all.js", "application/javascript")
        ]
#ifdef PRODUCTION
file (fp, ty) = do
    contents <- loadFile fp
    return EmbeddableEntry {eLocation = pack fp, eMimeType = ty, eContent = Left contents}
#else
file (fp, ty) =
    pure
        EmbeddableEntry
            {eLocation = pack fp, eMimeType = ty, eContent = Right [|loadFile fp|]}
#endif
