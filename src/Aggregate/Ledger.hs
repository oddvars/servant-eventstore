{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Aggregate.Ledger where

import           Aggregate.Class
import           Data.Aeson
import           Data.Text (pack)
import qualified Database.EventStore as E
import           GHC.Generics
import           Types
import           Store

data Ledger =
  Ledger { ledgerId :: LedgerId
         , ledgerCustomerId :: CustomerId
         } deriving Show

instance Aggregate Ledger where
  data AggregateCommand Ledger =
      CreateLedger LedgerId CustomerId
    | UpdateLedger LedgerId
    deriving Show

  data AggregateEvent Ledger =
      LedgerCreated LedgerId CustomerId
    | LedgerRevised LedgerId
    deriving (Generic, Show)

  data AggregateError Ledger =
      LedgerAlreadyExists
    | InvalidCreateLedgerCommand
    deriving Show

  apply state e _ = return (apply' state e)

  apply' state (LedgerCreated lid cid) = state { ledgerId = lid, ledgerCustomerId = cid}
  apply' state (LedgerRevised lid)     = state { ledgerId = lid }

  execute' _ (CreateLedger lid cid) = Right $ LedgerCreated lid cid
  execute' _ (UpdateLedger lid)     = Right $ LedgerRevised lid

  toESEvent e@LedgerCreated {} = createEvent "ledger-created" Nothing $ withJson e
  toESEvent e@LedgerRevised {} = createEvent "ledger-revised" Nothing $ withJson e

  seed uuid = Ledger (LedgerId (toUUID uuid)) (CustomerId emptyUUID)

  streamName l = "ledger:" <> pack lid
    where lid = show (ledgerId l)

instance FromJSON (AggregateEvent Ledger)
instance ToJSON   (AggregateEvent Ledger)

loadLedger :: E.Connection -> LedgerId -> IO Ledger
loadLedger conn lid =
  buildAggregate conn (seed lid)

newLedger :: E.Connection -> AggregateCommand Ledger -> IO (Either (AggregateError Ledger) Ledger)
newLedger _conn cmd@(CreateLedger lid _) =
  doCommand' _conn (seed lid :: Ledger) cmd E.noStreamVersion
newLedger _ _ = return $ Left InvalidCreateLedgerCommand
