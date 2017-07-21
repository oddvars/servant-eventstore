{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Aggregate.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import           Data.Maybe (fromJust)
import           Data.Text (pack, unpack)
import           Data.UUID
import qualified Database.EventStore as E
import           GHC.Generics
import           Servant


--------------------------------------------------------------------------------
-- CustomerId definition
--------------------------------------------------------------------------------
newtype CustomerId =
  CustomerId UUID
  deriving (Eq, Ord, Generic)

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

instance HasId CustomerId where
  toUUID (CustomerId uuid) = uuid
  fromUUID = CustomerId

--------------------------------------------------------------------------------
-- LedgerId definition
--------------------------------------------------------------------------------
newtype LedgerId =
  LedgerId UUID
  deriving (Eq, Ord)

instance FromHttpApiData LedgerId where
  parseUrlPiece x = case (fromString . unpack) x of
                      Nothing   -> Left "LedgerId"
                      (Just lid) -> Right (LedgerId lid)

instance Show LedgerId where
  show (LedgerId uuid) = show uuid

instance ToJSON LedgerId where
  toJSON (LedgerId uuid) = String $ pack $ toString uuid

instance FromJSON LedgerId where
  parseJSON (String str) =
    case fromString $ unpack str of
      Just uuid -> return $ LedgerId uuid
      _         -> typeMismatch "Invalid LedgerId UUID " (String str)
  parseJSON v = typeMismatch "Invalid LedgerId " v

instance HasId LedgerId where
  toUUID (LedgerId uuid) = uuid
  fromUUID = LedgerId

emptyUUID :: UUID
emptyUUID = fromJust (fromString "00000000-0000-0000-0000-000000000000")

data AppConfig =
  AppConfig
  { esConnection :: E.Connection
  , staticPath :: FilePath
  }
