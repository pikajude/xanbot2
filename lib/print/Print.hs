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
import System.Environment
import System.IO
import System.IO.Unsafe
import Text.PrettyPrint.ANSI.Leijen

data Sev
    = Debug
    | Info
    | Warn
    | Error
    deriving (Show, Lift, Eq)

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
            readChan ch >>= \Msg {..} -> do
                putDoc $
                    (if sev == Info
                         then id
                         else (docSev sev <+>)) $
                    docLoc loc <+> string msg <> hardline
                hFlush stdout
        return ch
  where
    docSev Debug = dullblue "DBG"
    docSev Info = dullgreen "INF"
    docSev Warn = dullyellow "WRN"
    docSev Error = dullred "ERR"
    docLoc (lp, lm) = brackets (h $ string lm)
      where
        h =
            [ dullblue
            , dullmagenta
            , dullcyan
            , bold . dullblue
            , bold . dullmagenta
            , bold . dullcyan
            ] !!
            abs (hash lp `mod` 6)

helper s = do
    l <- location
    let t = (loc_package l, loc_module l)
    [|\m ->
          liftIO $ do
              sp <- $(shouldPrint) s
              when sp $ writeChan _print (Msg m s t)|]
  where
    shouldPrint =
        [|\s -> do
              dbg <- lookupEnv "DEBUG"
              return $ s /= Debug || dbg == Just "1"|]

dbg = helper Debug

info = helper Info

warn = helper Warn

err = helper Error
