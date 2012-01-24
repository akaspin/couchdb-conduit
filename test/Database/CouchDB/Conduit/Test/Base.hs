{-# LANGUAGE OverloadedStrings #-} 

-- | base tests
module Database.CouchDB.Conduit.Test.Base (tests) where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import CouchDBAuth

import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as B
--import Data.Conduit
--import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB
import Database.CouchDB.Conduit.LowLevel

tests :: Test
tests = mutuallyExclusive $ testGroup "Base" [
    testCase "Just connect" case_justConnect,
    testCase "Put DB" case_dbPut
    ]

-- | Just connect
case_justConnect :: Assertion
case_justConnect = runCouch def $ do
    H.Response (HT.Status sc _) _h _bsrc <- couch' HT.methodGet "" [] [] 
                    (H.RequestBodyBS B.empty)
    liftIO $ sc @=? 200

-- | Put and delete
case_dbPut :: Assertion    
case_dbPut =  runCouch def {couchLogin = login, 
                            couchPass=pass,
                            couchDB="cdbc_dbputdel"} $ do
    couchPutDB_
    couchDeleteDB
    
    