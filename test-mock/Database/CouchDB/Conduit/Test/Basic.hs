{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.CouchDB.Conduit.Test.Basic (tests) where

import Prelude hiding (catch)

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import Control.Monad.Trans.Resource
import           Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Base (liftBase)
import Control.Exception (SomeException)
import Control.Exception.Lifted (catch, throwIO)
import Control.Applicative ((<$>), (<|>), optional, (*>), (<*), many)

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.Attoparsec
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Data.HashMap.Lazy as M

import Database.CouchDB.Conduit

tests :: Test
tests = testGroup "Couch mock" [
--    testCase "Outside couch" case_couchGet,
--    testCase "Protect" case_couchProtect,
--    testCase "Protect" case_couchProtect404
    testCase "Protect" case_couchSource
    ]


case_couchIn :: Assertion
case_couchIn = runCouch "localhost" 5984 "" $ do
    res <- runResourceT $ couch 
                HT.methodGet "" [] [] 
                handlerJ 
                (H.RequestBodyBS B.empty)
    liftBase $ print res

case_couchOut :: Assertion
case_couchOut = do 
    res <- runCouch "localhost" 5984 "" $ runResourceT $ couch 
            HT.methodGet "" [] [] 
            handlerJ 
            (H.RequestBodyBS B.empty)
    print res

case_couchProtect :: Assertion
case_couchProtect = do 
    res <- runCouch "localhost" 5984 "" $ runResourceT $ couch 
            HT.methodGet "" [] [] 
            (protect handlerJ) 
            (H.RequestBodyBS B.empty)
    print res

case_couchProtect404 :: Assertion
case_couchProtect404 = do 
    res <- runCouch "localhost" 5984 "non_exisi" $ runResourceT $ couch 
            HT.methodGet "" [] [] 
            (protect handlerJ) 
            (H.RequestBodyBS B.empty)
    print res

case_couchGet :: Assertion
case_couchGet = do 
    res <- runCouch "localhost" 5984 "" $ couchGetT "" []
    print res

case_couchSource :: Assertion
case_couchSource = do
    request <- H.parseUrl "http://hackage.haskell.org/packages/archive/conduit/0.0.0/doc/html/Data-Conduit.html"
    H.withManager $ \man -> do
         res <- H.http request handler man
         res $= CL.mapM print $$ CL.sinkNull
         return ()
  where
    handler _ _ bsrc = return $ bsrc 

    

handlerJ :: ResourceIO m => H.ResponseConsumer m A.Value
handlerJ _status _hdrs bsrc = bsrc $$ sinkParser A.json

handlerJC :: ResourceIO m => Conduit B.ByteString m A.Value
handlerJC = sequenceSink () $ \() ->
    Emit () . return <$> (sinkParser A.json)

couchGetT :: (MonadCouch m) => 
       B.ByteString
    -> HT.Query
    -> m A.Value
couchGetT p q = runResourceT $ couch HT.methodGet p [] q handlerJ 
            (H.RequestBodyBS B.empty) 

