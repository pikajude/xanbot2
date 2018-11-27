module Web.Static where

import           Data.ByteString.Lazy           ( fromStrict )
import           Data.FileEmbed
import           Data.Text                      ( pack )
import           Network.Mime
import           System.Directory.Tree hiding (file)
import           WaiAppStatic.Storage.Embedded
import           Lens.Micro.Platform
import           Web.Static.Hash                ( loadFile )

embedded = do
    tree <- build "static/"
    let paths = toListOf (traverse . _file) . flattenDir $ dirTree tree
    mapM (\x -> file (x, defaultMimeLookup (pack x))) paths

#ifdef PRODUCTION
file (fp, ty) = do
    contents <- loadFile fp
    return EmbeddableEntry {eLocation = pack (drop 7 fp), eMimeType = ty, eContent = Left contents}
#else
file (fp, ty) =
    pure
        EmbeddableEntry
            { eLocation = pack (drop 7 fp)
            , eMimeType = ty
            , eContent = Right [|loadFile fp|]
            }
#endif
