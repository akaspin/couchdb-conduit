{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}

module Database.CouchDB.Conduit.Test.Basic (tests) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class

import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Aeson as A
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU8

tests :: Test
tests = testGroup "Couch mock" [
--    testCase "Inside couch" case_couchIn,
--    testCase "Outside couch" case_couchOut,
--    testCase "Outside couch" case_couchGet
    ]

-- | Represents a single connection to CouchDB server. 
data CouchConnection = CouchConnection {
      host      :: B.ByteString     -- ^ Hostname
    , port      :: Int              -- ^ Port
    , manager   :: H.Manager        -- ^ Manager
    , dbname    :: String           -- ^ Database name
}

--type MonadCouch a = ResourceT (ReaderT CouchConnection a)

class (MonadBaseControl IO m, MonadIO m) => MonadCouch m where
    couchConnection :: m CouchConnection

instance (MonadBaseControl IO m, MonadIO m) =>
    MonadCouch (ReaderT CouchConnection m) 
    where
    couchConnection = ask
    
case_couchIn :: Assertion
case_couchIn = runCouch "localhost" 5984 "" $ do
    res <- couch HT.methodGet "" [] "" handlerJ (H.RequestBodyBS B.empty)
    liftBase $ print res

--case_couchOut :: Assertion
--case_couchOut = do 
--    res <- runCouch "localhost" 5984 "" $ couch HT.methodGet "" [] "" handlerJ (H.RequestBodyBS B.empty)
--    print res
--
--case_couchGet :: Assertion
--case_couchGet = do 
--    res <- runCouch "localhost" 5984 "" $ couchGet "" ""
--    print res

handlerJ :: ResourceIO m => H.ResponseConsumer m Value
handlerJ _status _hdrs bsrc = bsrc $$ sinkParser A.json



couchGet p q = couch HT.methodGet p [] q handlerJ 
            (H.RequestBodyBS B.empty) 

couch :: (MonadCouch (t (ResourceT m)), ResourceIO m, MonadTrans t) =>
            HT.Method
            -> String
            -> HT.RequestHeaders
            -> HT.Ascii
            -> H.ResponseConsumer m b
            -> H.RequestBody m
            -> t (ResourceT m) b
couch meth path hdrs qs acts reqBody = do
    conn <- couchConnection
    let req = H.def 
            { H.method      = meth
            , H.host        = host conn
            , H.requestHeaders = hdrs
            , H.port        = port conn
            , H.path        = BU8.fromString ("/" ++ dbname conn ++ "/" ++ path)
            , H.queryString = qs
            , H.requestBody = reqBody }
    lift $ H.http req acts (manager conn)


runCouch :: ResourceIO m =>
       String
    -> Int
    -> String
    -> ReaderT CouchConnection (ResourceT m) a
    -> m a
runCouch h p d = withCouchConnection h p d . runReaderT

withCouchConnection :: ResourceIO m =>
       String
    -> Int
    -> String
    -> (CouchConnection -> ResourceT m a)
    -> m a
withCouchConnection h p db f = 
     H.withManager $ \m -> f $ CouchConnection (BU8.fromString h) p m db
     
