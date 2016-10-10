{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
    ( startApp
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Database.EventStore as E
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.Hashable
import           Data.Text
import           Data.Time
import           Data.UUID
import           Data.UUID.V4
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Store

newtype CustomerId = CustomerId UUID deriving (Eq, Ord, Generic)

instance FromHttpApiData CustomerId where
  parseUrlPiece x = case (fromString . unpack) x of
                      Nothing   -> Left "CustomerId"
                      (Just id) -> Right (CustomerId id)

instance Show CustomerId where
  show (CustomerId uuid) = show uuid

instance Hashable CustomerId where
  hash (CustomerId uuid) = hash $ toASCIIBytes uuid

instance ToJSON CustomerId where
  toJSON (CustomerId uuid) = String $ pack $ toString uuid

instance FromJSON CustomerId where
  parseJSON (String str) =
    case fromString $ unpack str of
      Just uuid -> return $ CustomerId uuid
      _         -> typeMismatch "Invalid CustomerId UUID " (String str)
  parseJSON v = typeMismatch "Invalid CustomerId " v

customerStream :: CustomerId -> Text
customerStream (CustomerId uuid) = "customer:" E.<> pack (toString uuid)

data AppConfig = AppConfig
  { esConnection :: E.Connection }

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data CreateCustomerRequest = CreateCustomerRequest
  { createCustomerName :: Text
  , createCustomerAddress :: Text
  } deriving (Eq, Show)


data CustomerDTO = CustomerDTO
  { dtoCustomerId :: CustomerId
  , dtoCustomerName :: Text
  , dtoCustomerAddress :: Text
  } deriving (Eq, Show)

data CustomerCommand
  = CreateCustomer CustomerId Text Text
  | MoveCustomer CustomerId Text
  deriving (Eq, Show)

data CustomerEvent
  = CustomerCreated CustomerId Text Text
  | CustomerMoved
  deriving (Eq, Show)

data Customer
  = Customer
  { _conn :: E.Connection
  , _id :: CustomerId
  , _var :: TVar CustomerSnapshot
  }

data CustomerSnapshot
  = CustomerSnapshot
  { customerName :: Text
  , customerAddress :: Text
  }

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''CustomerEvent)
$(deriveJSON defaultOptions ''CustomerDTO)
$(deriveJSON defaultOptions ''CreateCustomerRequest)

type ApiHandler = ReaderT AppConfig Handler

readerToEither :: AppConfig -> ApiHandler :~> Handler
readerToEither cfg = Nat $ \x -> runReaderT x cfg

type API = "customer" :> Capture "customerId" CustomerId :>  Get '[JSON] CustomerDTO
      :<|> "customer" :> ReqBody '[JSON] CreateCustomerRequest :> Post '[JSON] NoContent

seed :: CustomerSnapshot
seed =  CustomerSnapshot "" ""

snapshot :: Customer -> IO CustomerSnapshot
snapshot Customer {..} = readTVarIO _var

apply :: CustomerSnapshot -> CustomerCommand -> CustomerSnapshot
apply s (CreateCustomer cid name address) = createCustomer s cid name address

newCustomer :: E.Connection -> CustomerCommand -> IO (Maybe Customer)
newCustomer _conn (CreateCustomer cid name address) = do
  let cmd = CreateCustomer cid name address
      evt = toEvent cmd
      _id = cid
  writeEvents <- E.sendEvent _conn (customerStream _id) E.noStreamVersion evt
  atomically $ do
    _ <- E.waitSTM writeEvents
    let p = apply seed cmd
    _var <- newTVar p
    return $ Just Customer {..}
newCustomer _conn _ = return Nothing

buildCustomer :: E.Connection -> CustomerId -> IO Customer
buildCustomer conn customerId = do
  s   <- streamFold conn (customerStream customerId) customer seed
  var <- newTVarIO s
  return $ Customer conn customerId var
  where
    customer s (CustomerCreated cid name address ) _ = return $ createCustomer s cid name address

createCustomer :: CustomerSnapshot -> CustomerId -> Text -> Text -> CustomerSnapshot
createCustomer snap cid name address =
  snap { customerName = name
       , customerAddress = address }

execute :: E.Connection -> CustomerId -> CustomerCommand -> IO ()
execute _conn _id cmd = do
  writeEvent <- E.sendEvent _conn (customerStream _id) E.anyVersion (toEvent cmd)
  atomically $ do
    _ <- E.waitSTM writeEvent
    return ()

toEvent :: CustomerCommand -> E.Event
toEvent (CreateCustomer customerId name address) =
  E.createEvent "customer-created" Nothing $
    E.withJson (CustomerCreated customerId name address)

startApp :: IO ()
startApp = do
  conn <- E.connect E.defaultSettings (E.Static "127.0.0.1" 1113)
  let cfg = AppConfig {esConnection = conn}
  run 9000 $ app cfg

api :: Proxy API
api = Proxy

app :: AppConfig -> Application
app cfg = serve api (apiServer cfg)

apiServer :: AppConfig -> Server API
apiServer cfg = enter (readerToEither cfg) server

server :: ServerT API ApiHandler
server = load :<|> create

load :: CustomerId -> ApiHandler CustomerDTO
load cid = do
  cfg <- ask
  snap <- liftIO $ buildCustomer (esConnection cfg) cid >>= snapshot
  return $ CustomerDTO cid (customerName snap) (customerAddress snap)


create :: CreateCustomerRequest -> ApiHandler NoContent
create (CreateCustomerRequest name address) = do
  cfg <- ask
  uuid <- liftIO nextRandom
  let customerId = CustomerId uuid
      cmd = CreateCustomer customerId name address
  _ <- liftIO $ execute (esConnection cfg) customerId cmd
  return NoContent
