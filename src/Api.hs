{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Aggregate.Class
import Aggregate.Customer
import Aggregate.Ledger
import Control.Monad.Reader
import Data.UUID.V4
import DTO
import Requests
import Servant
import Store
import Types

type ApiHandler = ReaderT AppConfig Handler

type API = "customer" :> Capture "customerId" CustomerId :>  Get '[JSON] CustomerDTO
      :<|> "customer" :> ReqBody '[JSON] CreateCustomerRequest :> Post '[JSON] (Maybe CustomerDTO)
      :<|> "customer" :> "move" :> ReqBody '[JSON] MoveCustomerRequest :> Post '[JSON] NoContent
      :<|> "customer" :> "attach" :> ReqBody '[JSON] AttachLedgerRequest :> Post '[JSON] NoContent
      :<|> "ledger"   :> Capture "ledgerId" LedgerId :> Get '[JSON] LedgerDTO
      :<|> "ledger"   :> ReqBody '[JSON] CreateLedgerRequest :> Post '[JSON] (Maybe LedgerDTO)

type WholeAPI = API :<|> "static" :> Raw

api :: Proxy API
api = Proxy

wholeApi :: Proxy WholeAPI
wholeApi = Proxy

app :: AppConfig -> Application
app cfg = serve wholeApi (apiServer cfg)

server :: ServerT API ApiHandler
server = getCustomer
    :<|> createCustomer
    :<|> moveCustomer
    :<|> attachLedger
    :<|> getLedger
    :<|> createLedger

apiServer :: AppConfig -> Server WholeAPI
apiServer cfg = enter (readerToEither cfg) server
           :<|> serveDirectory  (staticPath cfg)

readerToEither :: AppConfig -> ApiHandler :~> Handler
readerToEither cfg = Nat $ \x -> runReaderT x cfg

getCustomer :: CustomerId -> ApiHandler CustomerDTO
getCustomer cid = do
  cfg <- ask
  snap <- liftIO $ loadCustomer (esConnection cfg) cid
  return $ CustomerDTO cid (customerName snap) (customerAddress snap) (customerLedgerId snap)

createCustomer :: CreateCustomerRequest -> ApiHandler (Maybe CustomerDTO)
createCustomer (CreateCustomerRequest name address) = do
  cfg <- ask
  uuid <- liftIO nextRandom
  let cmd = CreateCustomer (CustomerId uuid) name address
  _ <- liftIO $ newCustomer (esConnection cfg) cmd
  return (Just (CustomerDTO (CustomerId uuid) name address Nothing))

moveCustomer :: MoveCustomerRequest -> ApiHandler NoContent
moveCustomer (MoveCustomerRequest cid address) = do
  cfg <- ask
  let cmd = RelocateCustomer cid address
  _ <- liftIO $ doCommand' (esConnection cfg) (seed cid) cmd anyVersion
  return NoContent

attachLedger :: AttachLedgerRequest -> ApiHandler NoContent
attachLedger (AttachLedgerRequest cid lid) = do
  cfg <- ask
  let cmd = AttachLedger cid lid
  _ <- liftIO $ doCommand' (esConnection cfg) (seed cid) cmd anyVersion
  return NoContent

getLedger :: LedgerId -> ApiHandler LedgerDTO
getLedger lid = do
  cfg <- ask
  snap <- liftIO $ loadLedger (esConnection cfg) lid
  return $ LedgerDTO lid (ledgerCustomerId snap)

createLedger :: CreateLedgerRequest -> ApiHandler (Maybe LedgerDTO)
createLedger (CreateLedgerRequest cid) = do
  cfg <- ask
  uuid <- liftIO nextRandom
  let lid = LedgerId uuid
      cmd = CreateLedger lid cid
  dto <- liftIO $ newLedger (esConnection cfg) cmd
  case dto of
    (Right d) -> do
      _ <- liftIO $ doCommand' (esConnection cfg) (seed cid) (AttachLedger cid lid) anyVersion
      return (Just (LedgerDTO (ledgerId d) cid))
    _         -> return Nothing
