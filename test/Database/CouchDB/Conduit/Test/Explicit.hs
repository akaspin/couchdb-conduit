{-# LANGUAGE OverloadedStrings #-} 

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Database.CouchDB.Conduit.Test.Explicit (tests) where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import Database.CouchDB.Conduit.Test.Util

import Control.Exception.Lifted (bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>), empty)

import Data.ByteString (ByteString)
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
    setup teardown $
    runCouch (conn dbName) $ do
        rev <- couchPut dbName "doc-just" "" [] $ TestDoc "doc" 1 "1"
        rev' <- couchPut dbName "doc-just" rev [] $ TestDoc "doc" 2 "2"
        rev'' <- couchRev dbName "doc-just"
        liftIO $ rev' @=? rev''
        couchDelete dbName "doc-just" rev''

case_massFlow :: Assertion
case_massFlow = bracket_
    setup teardown $
    runCouch (conn dbName) $ do
        revs <- mapM (\n -> 
                couchPut dbName (docn n) "" [] $ TestDoc "doc" n $ show n
            ) [1..100]
        liftIO $ length revs @=? 100
        revs' <- mapM (\n -> couchRev dbName $ docn n) [1..100]
        liftIO $ revs @=? revs'
        liftIO $ length revs' @=? 100
        mapM_ (\(n,r) ->
            couchDelete dbName (docn n) r) $ zip [1..100] revs'
  where
    docn n = fromString $ "doc-" ++ show (n :: Int)    


case_massIter :: Assertion
case_massIter = bracket_
    setup teardown $
    runCouch (conn dbName) $ 
        mapM_ (\n -> do
            let name = docn n 
            let d = TestDoc "doc" n $ show n
            rev <- couchPut dbName name "" [] d
            couchDelete dbName (docn n) rev
            _ <- couchPut dbName name "" [] d
            rev'' <- couchPut' dbName name [] d
            (_, d') <- couchGet dbName name []
            liftIO $ d @=? d'
            couchDelete dbName (docn n) rev''
         ) [1..100]
  where
    docn n = fromString $ "doc-" ++ show (n :: Int) 
    
setup :: IO ()
setup = setupDB dbName
teardown :: IO ()
teardown = tearDB dbName

dbName :: ByteString
dbName = "cdbc_test_explicit"   