{-# LANGUAGE TemplateHaskell #-}
module Requests where

import Data.Aeson.TH
import Data.Text
import Types

data CreateCustomerRequest = CreateCustomerRequest
  { createCustomerName :: Text
  , createCustomerAddress :: Text
  } deriving (Eq, Show)

data CreateLedgerRequest =
  CreateLedgerRequest
  { clrCustomerId :: CustomerId
  } deriving Show

data MoveCustomerRequest =
  MoveCustomerRequest
  { mcrCustomerId :: CustomerId
  , mcrAddress :: Text
  } deriving Show

data AttachLedgerRequest =
  AttachLedgerRequest
  { alrCustomerId :: CustomerId
  , alrLedgerId :: LedgerId
  } deriving Show

$(deriveJSON defaultOptions ''CreateCustomerRequest)
$(deriveJSON defaultOptions ''CreateLedgerRequest)
$(deriveJSON defaultOptions ''MoveCustomerRequest)
$(deriveJSON defaultOptions ''AttachLedgerRequest)
