{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Aggregate.Customer where

import           Control.Concurrent.STM
import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import           Data.UUID (toString)
import qualified Database.EventStore as E
import           Store
import           Types

data CustomerSnapshot
  = CustomerSnapshot
  { customerName :: Text
  , customerAddress :: Text
  }

data Customer
  = Customer
  { _conn :: E.Connection
  , _id :: CustomerId
  , _var :: TVar CustomerSnapshot
  }

customerStream :: CustomerId -> Text
customerStream (CustomerId uuid) = "customer:" <> pack (toString uuid)

seed :: CustomerSnapshot
seed =  CustomerSnapshot "" ""

snapshot :: Customer -> IO CustomerSnapshot
snapshot Customer {..} = readTVarIO _var

apply :: CustomerSnapshot -> CustomerCommand -> CustomerSnapshot
apply s (CreateCustomer cid name address) = createCustomer s cid name address
apply s _                                 = s

newCustomer :: E.Connection -> CustomerCommand -> IO (Maybe Customer)
newCustomer _conn (CreateCustomer cid name address) = do
  let cmd = CreateCustomer cid name address
      evt = toEvent cmd
      _id = cid
  writeEvents <- E.sendEvent _conn (customerStream _id) E.noStreamVersion evt
  _ <- E.waitAsync writeEvents
  atomically $ do
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
    customer s _                                   _ = return s

createCustomer :: CustomerSnapshot -> CustomerId -> Text -> Text -> CustomerSnapshot
createCustomer snap _ name address =
  snap { customerName = name
       , customerAddress = address }

execute :: E.Connection -> CustomerId -> CustomerCommand -> IO ()
execute _conn _id cmd = do
  let evt = toEvent cmd
  writeEvent <- E.sendEvent _conn (customerStream _id) E.anyVersion evt
  _ <- E.waitAsync writeEvent
  return ()

toEvent :: CustomerCommand -> E.Event
toEvent (CreateCustomer customerId name address) =
  E.createEvent "customer-created" Nothing $
    E.withJson (CustomerCreated customerId name address)

