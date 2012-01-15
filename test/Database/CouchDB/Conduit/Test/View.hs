{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE ScopedTypeVariables #-}

module Database.CouchDB.Conduit.Test.View where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import Database.CouchDB.Conduit.Test.Util (setupDB, tearDB)

--import Control.Monad.Trans.Class (lift)
import Control.Exception.Lifted (bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>), empty)

import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
--import qualified Data.Aeson.Generic as AG
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
    testCase "Create" case_createView,
    testCase "Big values parsing" case_bigValues,
    testCase "View with reduce" case_withReduce
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

case_createView :: Assertion
case_createView = bracket_
    (setupDB db)
    (tearDB db) $ runCouch (conn db) $ do
        rev <- couchViewPut' "mydesign" "myview"
            "function(doc){emit(null, doc);}" Nothing
        rev' <- CCG.couchRev "_design/mydesign"
        liftIO $ rev @=? rev' 
  where 
    db = "cdbc_test_view_create"

case_bigValues :: Assertion
case_bigValues = bracket_
    (runCouch (conn db) $ do
        couchPutDB ""
        _ <- couchViewPut' "mydesign" "myview"
                "function(doc){emit(doc.intV, doc);}" Nothing
        mapM_ (\n -> CCG.couchPut' (docName n) [] $ doc n) [1..20]
    )
    (tearDB db) $ runCouch (conn db) $ do
        res <- couchView' "mydesign" "myview" [] $ 
            (rowValue =$= CCG.toType) =$ CL.consume 
        mapM_ (\(a, b) -> liftIO $ a @=? doc b) $ zip res [1..20]
  where 
    db = "cdbc_test_view_big_values"
    doc n = T "doc" n $ concat $ replicate 10000 (show n)

data ReducedView = ReducedView Int deriving (Show, Eq, Data, Typeable)

case_withReduce :: Assertion    
case_withReduce = bracket_
    (runCouch (conn db) $ do
        couchPutDB ""
        _ <- couchViewPut' "mydesign" "myview"
                "function(doc){emit(doc.intV, doc.intV);}" 
                $ Just "function(keys, values){return sum(values);}"
        mapM_ (\n -> CCG.couchPut' (docName n) [] $ doc n) [1..20])
    (tearDB db) $ runCouch (conn db) $ do
        res <- couchView' "mydesign" "myview" [] $
            (rowValue =$= CCG.toType) =$ CL.consume
        liftIO $ res @=? [ReducedView 210]
  where
    db = "cdbc_test_view_reduce"
    doc n = T "doc" n $ show n
    
docName :: Int -> B.ByteString
docName n = fromString $ "doc" ++ show n    

-- | connection
conn :: Path -> CouchConnection
conn db = def {couchDB = db}
