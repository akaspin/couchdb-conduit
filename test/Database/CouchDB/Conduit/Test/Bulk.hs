{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-} 

-- LANGUAGE ScopedTypeVariables #-}

module Database.CouchDB.Conduit.Test.Bulk where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)
import Database.CouchDB.Conduit.Test.Util (tearDB, conn)

import Control.Exception.Lifted (bracket_)
import Control.Applicative ((<$>), (<*>), empty)

import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=))
import Data.Generics (Data, Typeable)

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Bulk
import Database.CouchDB.Conduit.DB


tests :: Test
tests = mutuallyExclusive $ testGroup "Bulk" [
    testCase "simple bulk post" caseSimpleBulkPost
    ]

data T = T {
    kind :: String,
    intV :: Int,
    strV :: String
    } deriving (Show, Eq, Data, Typeable)

instance A.FromJSON T where
   parseJSON (A.Object v) = T <$> v .: "kind" <*> v .: "intV" <*> v .: "strV"
   parseJSON _          = empty
   
instance A.ToJSON T where
   toJSON (T k i s) = A.object ["kind" .= k, "intV" .= i, "strV" .= s]

caseSimpleBulkPost :: Assertion
caseSimpleBulkPost = bracket_
    (runCouch conn $ couchPutDB_ db)
    (tearDB db) $ runCouch conn $ do
      couchPostBulk' db $ map doc [1..20]
  where
    db = "cdbc_test_bulk"
    doc n = T "doc" n $ show n
  
