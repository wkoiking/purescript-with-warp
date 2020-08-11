{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp ( run )
import Lib ( app )
import Control.Concurrent (MVar, newMVar)

-- | Just a server on port 8080
main :: IO ()
main = do
    let port = 8080
    vCounter <- newMVar 0 :: IO (MVar Int)
    putStrLn $ unwords ["Listening on port", show port]
    run port $ app vCounter

