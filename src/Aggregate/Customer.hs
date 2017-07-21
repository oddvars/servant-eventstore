{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Aggregate.Customer where

import           Aggregate.Class
import           Data.Aeson
import           Data.Text (Text, pack)
import qualified Database.EventStore as E
import           GHC.Generics
import           Store
import           Types

data Customer=
  Customer
  { customerId :: CustomerId
  , customerName :: Text
  , customerAddress :: Text
  , customerLedgerId :: Maybe LedgerId
  } deriving Show

instance Aggregate Customer where
  data AggregateCommand Customer =
      CreateCustomer CustomerId Text Text
    | RelocateCustomer CustomerId Text
    | AttachLedger CustomerId LedgerId
    deriving Show

  data AggregateEvent Customer =
      CustomerCreated CustomerId Text Text
    | CustomerRelocated CustomerId Text
    | LedgerAttached CustomerId LedgerId
    deriving (Generic, Show)

  data AggregateError Customer =
      CustomerExists
    | InvalidAddress
    | InvalidCreateCustomerCommand Text
    | LedgerAlreadyAttached
    deriving Show

  apply state e _ = return (apply' state e)

  apply' state (CustomerCreated cid n a) = state { customerId = cid, customerName = n, customerAddress = a }
  apply' state (CustomerRelocated _ a)   = state { customerAddress = a }
  apply' state (LedgerAttached _ lid)    = state { customerLedgerId = Just lid }

  execute' _ (CreateCustomer cid n a) = Right (CustomerCreated cid n a)
  execute' _ (RelocateCustomer cid a) = Right (CustomerRelocated cid a)
  execute' _ (AttachLedger cid lid)   = Right (LedgerAttached cid lid)

  toESEvent e@CustomerCreated {}   = createEvent "customer-created" Nothing $ withJson e
  toESEvent e@CustomerRelocated {} = createEvent "customer-relocated" Nothing $ withJson e
  toESEvent e@LedgerAttached {}    = createEvent "customer-ledger-attached" Nothing $ withJson e

  seed :: HasId b => b -> Customer
  seed uuid = Customer (CustomerId (toUUID uuid)) "" "" Nothing

  streamName c = "customer:" <> pack aggregateId
    where aggregateId = show (customerId c)

instance FromJSON (AggregateEvent Customer)
instance ToJSON   (AggregateEvent Customer)

loadCustomer :: E.Connection -> CustomerId -> IO Customer
loadCustomer conn cid =
  buildAggregate conn (seed cid)

newCustomer :: E.Connection -> AggregateCommand Customer -> IO (Either (AggregateError Customer) Customer)
newCustomer _conn cmd@(CreateCustomer _id _ _) =
  doCommand' _conn (seed _id :: Customer) cmd E.noStreamVersion
newCustomer _ cmd = return $ Left (InvalidCreateCustomerCommand (pack . show $ cmd))
