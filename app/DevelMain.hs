module DevelMain where

import Language.PureScript.Bridge
import Type
import Control.Concurrent
import Foreign.Store
-- import System.Process ( callCommand )
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Lib ( app )
import Control.Monad ( forM_ )

restart :: IO ()
restart = do
--     callCommand  "elm make elm-src/Main.elm"
    writePSTypes "purescript-src/src" (buildBridge defaultBridge) myTypes

    mtidStore <- lookupStore 0
    forM_ mtidStore $ \ tidStore -> do
        tid <- readStore tidStore
        killThread tid
    putStrLn "Forking server"
    newTid <- forkIO $ do
        vCounter <- newMVar 0
        run 8080 $ logStdoutDev $ app vCounter
    writeStore sTid newTid

-- | kill the server
shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore 0
    forM_ mtidStore $ \ tidStore -> do
        tid <- readStore tidStore
        killThread tid

sTid :: Store ThreadId
sTid = Store 0
