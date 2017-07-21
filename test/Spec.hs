{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Aggregate.Class
import Aggregate.Customer
import Data.UUID
import Data.UUID.V4
import Test.Hspec
import Types

main :: IO ()
main = testCustomerEvents

testCustomerEvents :: IO ()
testCustomerEvents =
  hspec $ do
    describe "test application of customer events to seed" $ do
      it "creates a customer" $ do
        uuid <- nextRandom
        let cid = fromUUID uuid :: CustomerId
            evt = CustomerCreated cid "Oddvar" "Street 1"
            agg = apply' (seed cid) evt
        customerName agg `shouldBe` "Oddvar"
        customerAddress agg `shouldBe` "Street 1"
      it "creates and relocates a customer" $ do
        uuid <- nextRandom
        let cid = fromUUID uuid :: CustomerId
            create = CustomerCreated cid "Oddvar" "Street 1"
            relocate = CustomerRelocated cid "Street 2"
            agg = foldl apply' (seed cid) [create, relocate]
        customerName agg `shouldBe` "Oddvar"
        customerAddress agg `shouldBe` "Street 2"

