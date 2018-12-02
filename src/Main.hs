{-# Language FlexibleContexts #-}
{-# Language NoMonomorphismRestriction #-}
{-# Language OverloadedStrings #-}
{-# Language RecursiveDo #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Reflex
import           Network.WebSockets
import           System.IO
import           Control.Monad.Fix
import           Data.Functor
import           Reflex.Host.Basic

main :: IO ()
main = basicHostForever $ mdo
    (pendingConn, bc) <- newTriggerEvent
    liftIO $ forkIO $ runServer "localhost" 8080 bc

    pb   <- getPostBuild
    echo <- line

    tck  <- tickLossyFromPostBuildTime 1

    performEvent_ $ liftIO . putStrLn <$> leftmost
        [echo, show <$> tck, show . pendingRequest <$> pendingConn]

    return ()

line = do
    pb <- getPostBuild
    performEventAsync $ pb $> \fn -> void $ liftIO $ forkIO $ fix $ \f -> do
        c <- hIsOpen stdin
        when c $ do
            fn =<< getLine
            f
