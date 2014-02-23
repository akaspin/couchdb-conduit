{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveDataTypeable #-} 

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Database.CouchDB.Conduit.Test.Generic (tests) where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import Database.CouchDB.Conduit.Test.Util

import Control.Exception.Lifted (bracket_)
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Generics (Data, Typeable)
import Data.String.Conversions (cs, (<>))
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Generic

tests :: Test
tests = mutuallyExclusive $ testGroup "Generic" [
    testCase "Just put-get-delete" caseJustPutGet,
    testCase "Mass flow" caseMassFlow,
    testCase "Mass Iter" caseMassIter
    ]
    
data TestDoc = TestDoc { kind :: String, intV :: Int, strV :: String } 
    deriving (Show, Eq, Data, Typeable)

caseJustPutGet :: Assertion
caseJustPutGet = bracket_
    setup teardown $ 
    runCouch conn $ do
        rev <- couchPut dbName "doc-just" "" [] $ TestDoc "doc" 1 "1"
        rev' <- couchPut dbName "doc-just" rev [] $ TestDoc "doc" 2 "2"
        rev'' <- couchRev dbName "doc-just"
        liftIO $ rev' @=? rev''
        couchDelete dbName "doc-just" rev''

caseMassFlow :: Assertion
caseMassFlow = bracket_
    setup teardown $ 
    runCouch conn $ do
        revs <- mapM (\n -> 
                couchPut dbName (docn n) "" [] $ TestDoc "doc" n $ show n
            ) [1..100]
        liftIO $ length revs @=? 100
        revs' <- mapM (couchRev dbName . docn) [1..100]
        liftIO $ revs @=? revs'
        liftIO $ length revs' @=? 100
        mapM_ (\(n,r) ->
            couchDelete dbName (docn n) r) $ zip [1..100] revs'
  where
    docn n = cs $ "doc-" <> show (n :: Int)    


caseMassIter :: Assertion
caseMassIter = bracket_
    setup teardown $ 
    runCouch conn $ 
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
    docn n = cs $ "doc-" <> show (n :: Int)   
    
setup :: IO ()
setup = setupDB dbName
teardown :: IO ()
teardown = tearDB dbName

dbName :: Text
dbName = "cdbc_test_generic"