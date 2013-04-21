{-# LANGUAGE OverloadedStrings #-} 

-- | base tests
module Database.CouchDB.Conduit.Test.Base (tests) where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import CouchDBAuth

import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as B

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB
import Database.CouchDB.Conduit.LowLevel

tests :: Test
tests = mutuallyExclusive $ testGroup "Base" [
    testCase "Just connect" caseJustConnect,
    testCase "Put and delete DB" caseDbPut
    ]

-- | Just connect
caseJustConnect :: Assertion
caseJustConnect = runCouch def $ do
    response <- couch HT.methodGet "" [] [] 
                    (H.RequestBodyBS B.empty) protect'
    liftIO $ (H.responseStatus response) @=? HT.status200

-- | Put and delete
caseDbPut :: Assertion    
caseDbPut =  runCouch def {couchLogin = login, 
                            couchPass=pass} $ do
    couchPutDB_ "cdbc_dbputdel"
    couchDeleteDB "cdbc_dbputdel"
    
    
