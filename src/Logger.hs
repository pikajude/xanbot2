{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Logger where

import           Text.PrettyPrint.ANSI.Leijen
import           Control.Monad.IO.Class
import           Reflex
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Lib
import           Server.Types

logWithFormat :: Q Exp
logWithFormat = do
  x <- qLocation
  let md = loc_module x
  [|\ f e -> performEvent_ $ fmap (liftIO . putDoc . (brackets (string $(stringE md)) <+>) . f) e|]

formatUserMessage :: Show a => (UniqueId, a) -> Doc
formatUserMessage (u, dm) =
  docUser u <+> blue "<<<" <+> string (show dm) <> hardline

formatUserErr :: Show a => (UniqueId, a) -> Doc
formatUserErr (u, exc) =
  docUser u <+> red "!!!" <+> string (show exc) <> hardline

formatServerMessage :: (UniqueId, Text) -> Doc
formatServerMessage (u, str) =
  docUser u <+> blue ">>>" <+> string (unpack str) <> hardline

docUser :: UniqueId -> Doc
docUser u = dullyellow (brackets ("user" <+> int (intId u)))
