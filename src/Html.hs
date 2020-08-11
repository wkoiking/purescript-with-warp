{-# LANGUAGE OverloadedStrings #-}

module Html where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Lucid
import Clay ( render )
import Css ( clayCss )

indexHtml :: MonadIO m => HtmlT m ()
indexHtml = do
    doctype_
    html_ $ do
        head_ $ do
            meta_ [charset_ "UTF-8"]
            title_ "PureScript with warp"
            -- recet css
            link_
                [ rel_ "stylesheet"
                , type_ "text/css"
                , href_ "https://cdnjs.cloudflare.com/ajax/libs/Primer/10.0.0-rc.21/build.css"
                ]
            link_
                [ rel_ "stylesheet"
                , type_ "text/css"
                , href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
                ]
            -- put css
            style_ [type_ "text/css"] $ render clayCss

--     <link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/Primer/10.0.0-rc.21/build.css" media="all">
--     <link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" media="all">
        body_ $ do
            pre_ [id_ "app"] ""
            -- put js
            js <- liftIO $ readFile "purescript-src/main.js"
            script_ [type_ "text/javascript"] js

