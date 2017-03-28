{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module App
    ( startApp
    ) where

import           Api
import qualified Database.EventStore as E
import           Network.Wai.Handler.Warp
import           Types

startApp :: IO ()
startApp = do
  conn <- E.connect E.defaultSettings (E.Static "127.0.0.1" 1113)
  let cfg = AppConfig {esConnection = conn, staticPath = "./static"}
  run 9000 $ app cfg
