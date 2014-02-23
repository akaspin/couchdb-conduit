{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE ScopedTypeVariables #-}

module Database.CouchDB.Conduit.Test.View where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import Database.CouchDB.Conduit.Test.Util (tearDB, conn)

--import Control.Monad.Trans.Class (lift)
import Control.Exception.Lifted (bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>), empty)

import qualified Data.ByteString as B
import qualified Data.Text as T

import Data.String.Conversions ((<>), cs)
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=))
import Data.Generics (Data, Typeable)
import Data.Conduit
import qualified Data.Conduit.List as CL

import Database.CouchDB.Conduit
import qualified Database.CouchDB.Conduit.Generic as CCG
import Database.CouchDB.Conduit.DB
import Database.CouchDB.Conduit.View
import Database.CouchDB.Conduit.Design

tests :: Test
tests = mutuallyExclusive $ testGroup "View" [
    testCase "Big values parsing" caseBigValues,
    testCase "With reduce" caseWithReduce,
    testCase "update_seq before rows" caseUpdateSeqTop,
    testCase "update_seq after rows" caseUpdateSeqAfter
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

caseBigValues :: Assertion
caseBigValues = bracket_
    (runCouch conn $ do
        couchPutDB_ db
        couchPutView db "mydesign" "myview"
                "function(doc){emit(doc.intV, doc);}" Nothing
        mapM_ (\n -> CCG.couchPut' db (docName n) [] $ doc n) [1..200]
    )
    (tearDB db) $ runCouch conn $ do
        res <- couchView_ db "mydesign" "myview" [] $ 
            (rowValue =$= CCG.toType) =$ CL.consume 
        mapM_ (\(a, b) -> liftIO $ a @=? doc b) $ zip res [1..20]
  where 
    db = "cdbc_test_view_big_values"
    doc n = T "doc" n $ concat $ replicate 10000 (show n)

data ReducedView = ReducedView Int deriving (Show, Eq, Data, Typeable)

caseWithReduce :: Assertion    
caseWithReduce = bracket_
    (runCouch conn $ do
        couchPutDB_ db
        couchPutView db "mydesign" "myview"
                "function(doc){emit(doc.intV, doc.intV);}" 
                $ Just "function(keys, values){return sum(values);}"
        mapM_ (\n -> CCG.couchPut' db (docName n) [] $ doc n) [1..20])
    (tearDB db) $ runCouch conn $ do
        res <- couchView_ db "mydesign" "myview" [] $
            (rowValue =$= CCG.toType) =$ CL.consume
        liftIO $ res @=? [ReducedView 210]
  where
    db = "cdbc_test_view_reduce"
    doc n = T "doc" n $ show n

caseUpdateSeqTop :: Assertion
caseUpdateSeqTop = bracket_
    (runCouch conn $ do
        couchPutDB_ db
        couchPutView db "mydesign" "myview"
                "function(doc){emit(doc.intV, doc.intV);}" Nothing
        mapM_ (\n -> CCG.couchPut' db (docName n) [] $ doc n) [1..20])
    (tearDB db) $ runCouch conn $ do
        res <- couchView_ db "mydesign" "myview" 
            [("update_seq",Just "true"),("key",Just "1")] $
            (rowValue =$= CCG.toType) =$ CL.consume
        liftIO $ res @=? [ReducedView 1]
  where
    db = "cdbc_test_view_before"
    doc n = T "doc" n $ show n

caseUpdateSeqAfter :: Assertion
caseUpdateSeqAfter = bracket_
    (runCouch conn $ do
        couchPutDB_ db
        couchPutView db "mydesign" "myview"
                "function(doc){emit([doc.intV,doc.intV], doc.intV);}" Nothing
        mapM_ (\n -> CCG.couchPut' db (docName n) [] $ doc n) [1..20])
    (tearDB db) $ runCouch conn $ do
        res <- couchView_ db "mydesign" "myview" 
            [("keys",Just "[[0,0]]")] $
            (rowValue =$= CCG.toType) =$ CL.consume
        liftIO $ res @=? ([] :: [ReducedView])
        
        
  where
    db = "cdbc_test_view_after"
    doc n = T "doc" n $ show n
  
docName :: Int -> T.Text
docName n = cs $ "doc" <> show n    
