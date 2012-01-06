{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Test.Explicit where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import Database.CouchDB.Conduit.Test.Util

import Control.Exception.Lifted (bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>), empty)

import Data.Aeson
import Data.ByteString.UTF8 (fromString)
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Explicit

tests :: Test
tests = mutuallyExclusive $ testGroup "Explicit" [
    testCase "Just put-get-delete" case_justPutGet,
    testCase "Mass flow" case_massFlow,
    testCase "Mass Iter" case_massIter
    ]
    
data TestDoc = TestDoc { kind :: String, intV :: Int, strV :: String } 
    deriving (Show, Eq)

instance FromJSON TestDoc where
   parseJSON (Object v) = TestDoc <$> v .: "kind" <*> v .: "intV" <*> v .: "strV"
   parseJSON _          = empty
   
instance ToJSON TestDoc where
   toJSON (TestDoc k i s) = object ["kind" .= k, "intV" .= i, "strV" .= s]

case_justPutGet :: Assertion
case_justPutGet = bracket_
    (setupDB "cdbc_test_basic")
    (tearDB "cdbc_test_basic") $ 
    runCouch "localhost" 5984 "cdbc_test_basic" $ do
        rev <- couchPut "doc-just" "" [] $ TestDoc "doc" 1 "1"
        rev' <- couchPut "doc-just" rev [] $ TestDoc "doc" 2 "2"
        rev'' <- couchRev "doc-just"
        liftIO $ rev' @=? rev''
        couchDelete "doc-just" rev''

case_massFlow :: Assertion
case_massFlow = bracket_
    (setupDB "cdbc_test_basic")
    (tearDB "cdbc_test_basic") $ 
    runCouch "localhost" 5984 "cdbc_test_basic" $ do
        revs <- mapM (\n -> 
                couchPut (docn n) "" [] $ TestDoc "doc" n $ show n
            ) [1..100]
        liftIO $ length revs @=? 100
        revs' <- mapM (\n ->
            couchRev $ docn n) [1..100]
        liftIO $ revs @=? revs'
        liftIO $ length revs' @=? 100
        mapM_ (\(n,r) ->
            couchDelete (docn n) r) $ zip [1..100] revs'
  where
    docn n = fromString $ "doc-" ++ show (n :: Int)    


case_massIter :: Assertion
case_massIter = bracket_
    (setupDB "cdbc_test_basic")
    (tearDB "cdbc_test_basic") $ 
    runCouch "localhost" 5984 "cdbc_test_basic" $ 
        mapM_ (\n -> do
            let name = docn n 
            let d = TestDoc "doc" n $ show n
            rev <- couchPut name "" [] d
            couchDelete (docn n) rev
            _ <- couchPut name "" [] d
            rev'' <- couchPut' name [] d
            d' <- couchGet name []
            liftIO $ d @=? d'
            couchDelete (docn n) rev''
         ) [1..100]
  where
    docn n = fromString $ "doc-" ++ show (n :: Int)    