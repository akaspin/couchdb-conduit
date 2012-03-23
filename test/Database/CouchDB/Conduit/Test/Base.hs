{-# LANGUAGE OverloadedStrings #-} 

-- | base tests
module Database.CouchDB.Conduit.Test.Base (tests) where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import CouchDBAuth

import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as B
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB
import Database.CouchDB.Conduit.LowLevel

tests :: Test
tests = mutuallyExclusive $ testGroup "Base" [
    testCase "Just HTTP" caseJustHttp,
    testCase "Just HTTP IO" caseJustHttpIO,
    testCase "Just connect" caseJustConnect,
    testCase "Put and delete DB" caseDbPut
    ]
--    testCase "Put DB" case_dbPut

caseJustHttp :: Assertion
caseJustHttp = do
    request <- H.parseUrl "http://google.com/"
    r <- H.withManager $ \manager -> do
          H.Response _ _ _ bsrc <- H.http request manager
          bsrc $$ CL.consume
    print r

caseJustHttpIO :: Assertion
caseJustHttpIO = do
    request <- H.parseUrl "http://google.com/"
    H.withManager $ \manager -> do
          H.Response _ _ _ bsrc <- H.http request manager
          r <- bsrc $$ CL.consume
          liftIO $ print r

-- | Just connect
caseJustConnect :: Assertion
caseJustConnect = runCouch def $ do
    H.Response (HT.Status sc _) _ _h _bsrc <- couch HT.methodGet "" [] [] 
                    (H.RequestBodyBS B.empty) protect'
    liftIO $ sc @=? 200

-- | Put and delete
caseDbPut :: Assertion    
caseDbPut =  runCouch def {couchLogin = login, 
                            couchPass=pass} $ do
    couchPutDB_ "cdbc_dbputdel"
    couchDeleteDB "cdbc_dbputdel"
    
    