{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Hashable
import           Data.Text (Text, pack, unpack)
import           Data.UUID
import qualified Database.EventStore as E
import           GHC.Generics
import           Servant

newtype CustomerId = CustomerId UUID deriving (Eq, Ord, Generic)

instance FromHttpApiData CustomerId where
  parseUrlPiece x = case (fromString . unpack) x of
                      Nothing   -> Left "CustomerId"
                      (Just cid) -> Right (CustomerId cid)

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


data AppConfig = AppConfig
  { esConnection :: E.Connection
  , staticPath :: FilePath }

data CreateCustomerRequest = CreateCustomerRequest
  { createCustomerName :: Text
  , createCustomerAddress :: Text
  } deriving (Eq, Show)

data CustomerCommand
  = CreateCustomer CustomerId Text Text
  | MoveCustomer CustomerId Text
  deriving (Eq, Show)

data CustomerDTO = CustomerDTO
  { dtoCustomerId :: CustomerId
  , dtoCustomerName :: Text
  , dtoCustomerAddress :: Text
  } deriving (Eq, Show)

data CustomerEvent
  = CustomerCreated CustomerId Text Text
  | CustomerMoved
  deriving (Eq, Show)


$(deriveJSON defaultOptions ''CreateCustomerRequest)
$(deriveJSON defaultOptions ''CustomerDTO)
$(deriveJSON defaultOptions ''CustomerEvent)
