{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Aggregate.Customer
import Control.Monad.Reader
import Data.UUID.V4
import Servant
import Types

type ApiHandler = ReaderT AppConfig Handler

type API = "customer" :> Capture "customerId" CustomerId :>  Get '[JSON] CustomerDTO
      :<|> "customer" :> ReqBody '[JSON] CreateCustomerRequest :> Post '[JSON] NoContent

api :: Proxy API
api = Proxy

server :: ServerT API ApiHandler
server = load :<|> create

apiServer :: AppConfig -> Server API
apiServer cfg = enter (readerToEither cfg) server

readerToEither :: AppConfig -> ApiHandler :~> Handler
readerToEither cfg = Nat $ \x -> runReaderT x cfg

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
