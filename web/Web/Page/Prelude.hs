{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Page.Prelude
    ( module Web.Page.Prelude
    , module Control.Monad.Reader
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Types.Status
import Network.Wai (Request, Response, responseLBS)
import Text.Blaze.Html.Renderer.Utf8
import Text.Hamlet

import qualified Store
import qualified Web.Session as Session

data Page = Page
    { title :: Maybe Text
    , status :: Maybe Status
    } deriving (Show)

setTitle t = modify (\p -> p {title = Just t})

setStatus s = modify (\p -> p {status = Just s})

layout :: (MonadIO m, MonadReader Request m) => StateT Page m Html -> m Response
layout page = do
    currentUser <- Session.getUser
    (html, Page {..}) <- runStateT page (Page Nothing Nothing)
    let nums = [1..2 :: Int]
    return $
        responseLBS
            (fromMaybe status200 status)
            [("Content-Type", "text/html;charset=utf-8")] $
        renderHtml
            [shamlet|
            !!!
            <html lang=en>
              <head>
                <link rel=stylesheet type="text/css" href="/css/foundation.min.css">
                <link rel=stylesheet type="text/css" href="/css/main.css">
                <title>
                  xanbot
                  $maybe st <- title
                    \ | #{st}
              <body>
                <div .container>
                  <div .navbar>
                    <a #home-button href=/>&#x1f916;
                  <div .main>
                    $maybe user <- currentUser
                      <div .header>
                        <a .header-user href=#>
                          <img .header-user-icon src=#{Store.logo user} alt="#{Store.display_name user}'s logo">
                          #{Store.display_name user}
                    ^{html}
            |]
