{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards   #-}

module App
    ( startApp
    ) where

import           Api
import qualified Database.EventStore as E
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Types

startApp :: IO ()
startApp = do
  conn <- E.connect E.defaultSettings (E.Static "127.0.0.1" 1113)
  let cfg = AppConfig {esConnection = conn}
  run 9000 $ app cfg

app :: AppConfig -> Application
app cfg = serve api (apiServer cfg)

