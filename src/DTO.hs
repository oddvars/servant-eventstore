{-# LANGUAGE TemplateHaskell #-}

module DTO where

import Data.Aeson.TH
import Data.Text
import Types

data CustomerDTO =
  CustomerDTO
  { dtoCustomerId :: CustomerId
  , dtoCustomerName :: Text
  , dtoCustomerAddress :: Text
  , dtoCustomerLedgerId :: Maybe LedgerId
  } deriving (Show)

data LedgerDTO =
  LedgerDTO
  { dtoLedgerId :: LedgerId
  , dtoLedgerCustomterId :: CustomerId
  } deriving (Show)

$(deriveJSON defaultOptions ''CustomerDTO)
$(deriveJSON defaultOptions ''LedgerDTO)
