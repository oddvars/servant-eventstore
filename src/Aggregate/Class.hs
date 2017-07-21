{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Aggregate.Class
  ( module ES
  , Aggregate
  , AggregateCommand
  , AggregateError
  , AggregateEvent
  , ExecutionResult
  , ExecutionResult'
  , HasId
  , apply
  , apply'
  , fromUUID
  , toUUID
--  , execute
  , execute'
  , seed
  , streamName
  , toESEvent
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Data.UUID
import Database.EventStore as ES

type ExecutionResult a = Either (AggregateError a) ES.Event
type ExecutionResult' a = Either (AggregateError a) (AggregateEvent a)

class HasId a where
 toUUID :: a -> UUID
 fromUUID :: UUID -> a

class Aggregate a where
  data AggregateCommand a :: *
  data AggregateEvent a   :: *
  data AggregateError a   :: *

--  execute    :: AggregateCommand a -> ExecutionResult a
  execute'   :: a -> AggregateCommand a -> ExecutionResult' a
  apply      :: (FromJSON (AggregateEvent a)) => a -> AggregateEvent a -> Maybe UTCTime -> IO a
  apply'     :: (FromJSON (AggregateEvent a)) => a -> AggregateEvent a -> a
  seed       :: HasId b => b -> a
  streamName :: a -> Text
  toESEvent    :: AggregateEvent a -> ES.Event
