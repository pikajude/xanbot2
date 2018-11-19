{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Print
    ( module Print
    , module X
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Hashable
import Data.String.Interpolate as X
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Text.PrettyPrint.ANSI.Leijen

data Sev
    = Info
    | Warn
    | Error
    deriving (Show, Lift)

data Msg = Msg
    { msg :: String
    , sev :: Sev
    , loc :: (String, String)
    } deriving (Show)

{-# NOINLINE _print #-}
_print :: Chan Msg
_print =
    unsafePerformIO $ do
        ch <- newChan
        forkIO $
            forever $
            readChan ch >>= \Msg {..} ->
                putDoc $ docSev sev <+> docLoc loc <+> string msg <> hardline
        return ch
  where
    docSev Info = dullgreen "INF"
    docSev Warn = dullyellow "WRN"
    docSev Error = dullred "ERR"
    docLoc (lp, lm) = brackets (h $ string str)
      where
        str = lp ++ ":" ++ lm
        h = [dullblue, dullmagenta, dullcyan] !! abs (hash lm `mod` 3)

helper s = do
    l <- location
    let t = (loc_package l, loc_module l)
    [|\m -> liftIO (writeChan _print (Msg m s t))|]

info = helper Info

warn = helper Warn

err = helper Error
