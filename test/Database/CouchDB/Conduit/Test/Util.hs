{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Test.Util where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)

import Data.ByteString

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB

-- | TestGroup with setup and teardown
hardenTest :: 
       String   -- ^ name 
    -> IO ()     -- ^ setup
    -> IO ()     -- ^ teardown
    -> Test     -- ^ Tests 
    -> Test
hardenTest name setup teardown pending = mutuallyExclusive $ testGroup name [
    testCase (name ++ " (setup)") setup,
    pending,
    testCase (name ++ " (teardown)") teardown
    ]

setupDB :: ByteString -> IO ()
setupDB n = runCouch "localhost" 5984 n $ couchPutDB ""

tearDB :: ByteString -> IO ()
tearDB n = runCouch "localhost" 5984 n $ couchDeleteDB ""
