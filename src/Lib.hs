{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.Wai ( Application, responseLBS, responseFile, requestMethod, getRequestBodyChunk  )
import Network.HTTP.Types ( status200, status404 )
import Network.HTTP.Types.Method ( methodGet )
import qualified Data.ByteString.Lazy.Char8 as LB ( pack )
import qualified Data.ByteString.Char8 as B ( putStrLn, null )
import Control.Concurrent (MVar, swapMVar, readMVar)
import Type as HT
import Argonaut (decodeArgonaut, encodeArgonaut)
import Html ( indexHtml )
import Lucid ( renderBST )

app :: MVar Int -> Application
app vCounter request respond = do
    putStrLn "Got http request"
    let methodBstr = requestMethod request
    B.putStrLn $ "Method = " <> methodBstr
    bodyBstr <- getRequestBodyChunk request
    putStrLn "Contents in body:"
    if B.null bodyBstr
        then putStrLn "No contents in body!!!"
        else B.putStrLn bodyBstr
    putStrLn ""
    if methodBstr == methodGet
        then do
            htmlBstr <- renderBST indexHtml
            respond $ responseLBS status200 [("Content-Type", "text/html")] htmlBstr
--             responseFile status200 [("Content-Type", "text/html")] "purescript-src/index.html" Nothing
        else case decodeArgonaut bodyBstr of
            Left err -> respond $ responseLBS status404 [("Content-Type", "text/plain")] (LB.pack err)
            Right elmMsg -> case elmMsg of
                UpdateCounterValue n -> do
                    _ <- swapMVar vCounter n
                    putStrLn $ "Counter updated to: " ++ show n
                    respond $ responseLBS status200 [] (encodeArgonaut $ HT.CurrentCounterValue n)
                RequestInitialValue -> do
                    n <- readMVar vCounter
                    putStrLn $ "Send initial counter value: " ++ show n
                    respond $ responseLBS status200 [] (encodeArgonaut $ HT.CurrentCounterValue n)
