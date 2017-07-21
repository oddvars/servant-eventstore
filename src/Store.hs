{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies      #-}
module Store (pushEvent, buildAggregate, doCommand') where

------------------------------------------------------------------------------
import Prelude
import Control.Monad

-------------------------------------------------------------------------------
import Control.Concurrent.STM
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Time hiding (parseTime)
import Database.EventStore
import Aggregate.Class

doCommand' :: Aggregate e => Connection -> e -> AggregateCommand e -> ExpectedVersion -> IO (Either (AggregateError e) e)
doCommand' _conn s cmd v =
  case execute' s cmd of
             (Right evt) -> pushEvent _conn s (toESEvent evt) v
             (Left err) -> return $ Left err

pushEvent :: Aggregate a => Connection -> a -> Event -> ExpectedVersion -> IO (Either (AggregateError a) a)
pushEvent _conn agg evt v = do
  writeEvent <- sendEvent _conn (streamName agg) v evt
  _ <- waitAsync writeEvent
  return (Right agg)

buildAggregate :: (FromJSON (AggregateEvent a), Aggregate a) => Connection -> a -> IO a
buildAggregate conn agg = streamFold conn (streamName agg) apply agg

------------------------------------------------------------------------------
parseTime :: ByteString -> Maybe UTCTime
parseTime bs =
    case parseOnly action bs of
      Left _  -> Nothing
      Right a -> Just a
  where
    action = do
        v <- value
        case fromJSON v of
          Error e   -> fail e
          Success a -> return a

-----------------------------------------------------------------------------
-- | Left fold a stream of events from the begining given a state seed.
streamFold :: (FromJSON (AggregateEvent a))
           => Connection
           -> Text
           -> (s -> AggregateEvent a -> Maybe UTCTime -> IO s)
           -> s
           -> IO s
streamFold conn stream k = go 0
  where
    go start s = do
        act <- readStreamEventsForward conn stream start 500 False
        res <- waitAsync act
        case res of
            ReadSuccess sl -> do
                let foldF ss revt =
                        let action = do
                                 let evt = resolvedEventOriginal revt
                                 meta <- recordedEventMetadata evt
                                 e    <- recordedEventDataAsJson evt
                                 return (parseTime meta, e) in
                        case action of
                            Nothing        -> return ss
                            Just (date, e) -> k ss e date
                    next = sliceNext sl
                newS <- foldM foldF s $ sliceEvents sl
                if sliceEOS sl
                    then return newS
                    else go next newS
            ReadNoStream -> return s
            e            -> fail $ "wrong slice read result" ++ show e

-----------------------------------------------------------------------------
forSub :: FromJSON a
       => Subscription s
       -> (a -> Maybe UTCTime -> IO ())
       -> IO ()
forSub sub k = forever $ do
    evt <- nextEvent sub
    let action = do
            a    <- resolvedEventDataAsJson evt
            meta <- recordedEventMetadata $ resolvedEventOriginal evt
            return (a, parseTime meta)
    traverse_ (uncurry k) action
