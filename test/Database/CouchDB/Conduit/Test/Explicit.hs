{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Test.Explicit where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import Database.CouchDB.Conduit.Test.Util

import Control.Monad.IO.Class (liftIO)

import Data.Aeson
import Data.ByteString.UTF8 (fromString)
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Explicit

tests :: Test
tests = mutuallyExclusive $ testGroup "Explicit" [
    testCase "Just put-delete" case_justPutGet,
    testCase "Just put-delete" case_massFlow
    ]
    
case_justPutGet :: Assertion
case_justPutGet = do
    setupDB "cdbc_test_basic"
    runCouch "localhost" 5984 "cdbc_test_basic" $ do
        rev <- couchPut "doc-just" "" [] $ object [ 
                    "kind" .= ("doc" :: String),
                    "intV" .= (1 :: Int), 
                    "strV" .= ("1" :: String) ]
        rev' <- couchPut "doc-just" rev [] $ object [ 
                    "kind" .= ("doc" :: String),
                    "intV" .= (2 :: Int), 
                    "strV" .= ("3" :: String) ]
        rev'' <- couchRev "doc-just"
        liftIO $ rev' @=? rev''
        couchDelete "doc-just" rev''
    tearDB "cdbc_test_basic"

case_massFlow :: Assertion
case_massFlow = do
    setupDB "cdbc_test_basic"
    runCouch "localhost" 5984 "cdbc_test_basic" $ do
        revs <- mapM (\n -> 
            couchPut (docn n) "" [] $ object [ 
                    "kind" .= ("doc" :: String),
                    "intV" .= (n :: Int), 
                    "strV" .= (show n :: String) ]) [1..100]
        revs' <- mapM (\n ->
            couchRev $ docn n) [1..100]
        liftIO $ revs @=? revs'
        mapM_ (\(n,r) ->
            couchDelete (docn n) r) $ zip [1..100] revs'
    tearDB "cdbc_test_basic"
  where
    docn n = fromString $ "doc-" ++ show (n :: Int)    