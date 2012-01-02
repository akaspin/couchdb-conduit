{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

module Database.CouchDB.Conduit.Test.Basic (tests) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class

import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT


import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Blaze.ByteString.Builder as BLB

tests :: Test
tests = testGroup "Couch mock" [
--    testCase "Inside couch" case_couchIn,
--    testCase "Outside couch" case_couchOut,
    testCase "Outside couch" case_couchGet
    ]

-- | Represents a single connection to CouchDB server. 
data CouchConnection = CouchConnection {
      host      :: B.ByteString     -- ^ Hostname
    , port      :: Int              -- ^ Port
    , manager   :: H.Manager        -- ^ Manager
    , dbname    :: B.ByteString           -- ^ Database name
}

class ResourceIO m => MonadCouch m where
    couchConnection :: m CouchConnection

instance ResourceIO m => MonadCouch (ReaderT CouchConnection m) where
    couchConnection = ask
    
case_couchIn :: Assertion
case_couchIn = runCouch "localhost" 5984 "" $ do
    res <- runResourceT $ couch HT.methodGet "" [] [] handlerJ (H.RequestBodyBS B.empty)
    liftBase $ print res

case_couchOut :: Assertion
case_couchOut = do 
    res <- runCouch "localhost" 5984 "" $ runResourceT $ couch HT.methodGet "" [] [] handlerJ (H.RequestBodyBS B.empty)
    print res

case_couchGet :: Assertion
case_couchGet = do 
    res <- runCouch "localhost" 5984 "" $ couchGet "" ""
    print res

handlerJ :: ResourceIO m => H.ResponseConsumer m A.Value
handlerJ _status _hdrs bsrc = bsrc $$ sinkParser A.json

couchGet :: (MonadCouch m) => 
       B.ByteString
    -> B.ByteString 
    -> m A.Value
couchGet p q = runResourceT $ couch HT.methodGet p [] [] handlerJ 
            (H.RequestBodyBS B.empty) 

couch :: (MonadCouch m) =>
           HT.Method                -- ^ method
        -> B.ByteString                   -- ^ path
        -> HT.RequestHeaders        -- ^ headers
        -> HT.Query                 
        -> H.ResponseConsumer m b
        -> H.RequestBody m
        -> ResourceT m b
couch meth path hdrs qs acts reqBody = do
    conn <- lift couchConnection
    let req = H.def 
            { H.method          = meth
            , H.host            = host conn
            , H.requestHeaders  = hdrs
            , H.port            = port conn
            , H.path            = B.intercalate "/" . filter (/="") $ [dbname conn, path]
            , H.queryString     = HT.renderQuery False qs
            , H.requestBody     = reqBody }
    H.http req acts (manager conn)

runCouch :: ResourceIO m =>
       B.ByteString
    -> Int
    -> B.ByteString
    -> ReaderT CouchConnection m a
    -> m a
runCouch h p d = withCouchConnection h p d . runReaderT

withCouchConnection :: ResourceIO m =>
       B.ByteString
    -> Int
    -> B.ByteString
    -> (CouchConnection -> m a)
    -> m a
withCouchConnection h p db f = 
     H.withManager $ \m -> lift $ f $ CouchConnection h p m db