{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Database.EventStore as E
import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant



data AppConfig = AppConfig
  { esConnection :: E.Connection
  }

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data CustomerEvent
  = CustomerCreated
  | AddressUpdate
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''CustomerEvent)

type ApiHandler = ReaderT AppConfig Handler

readerToEither :: AppConfig -> ApiHandler :~> Handler
readerToEither cfg = Nat $ \x -> runReaderT x cfg

type API = "user" :> Get '[JSON] User
      :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] NoContent

startApp :: IO ()
startApp = do
  conn <- E.connect E.defaultSettings (E.Static "127.0.0.1" 1113)
  let cfg = AppConfig {esConnection = conn}
  run 8080 $ app cfg

app :: AppConfig -> Application
app cfg = serve api (apiServer cfg)

api :: Proxy API
api = Proxy

apiServer :: AppConfig -> Server API
apiServer cfg = enter (readerToEither cfg) server

server :: ServerT API ApiHandler
server = load :<|> create

load :: ApiHandler User
load = return $ User 1 "Isaac" "Newton"

create :: User -> ApiHandler NoContent
create u = return NoContent
