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

import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Data.HashMap.Lazy as M

import Database.CouchDB.Conduit

tests :: Test
tests = testGroup "Couch mock" [
    testCase "Protect" case_couchSource,
    testCase "Rev" case_couchRev
    ]

case_couchRev :: Assertion
case_couchRev = runCouch "localhost" 5984 "testcouchenum" $ do
    res <- runResourceT $ couch HT.methodGet "otest-1" [] [] 
            (protect syncRev) 
            (H.RequestBodyBS B.empty)
    liftBase $ print res


syncRev _s h _bsrc = return $ (B.tail . B.init . fromJust $ lookup "Etag" h)

case_couchSource :: Assertion
case_couchSource = do
    request <- H.parseUrl "http://localhost:5984"
    H.withManager $ \man -> do
         res <- H.http request handler man
         res $= CL.mapM print $$ CL.sinkNull
         return ()
  where
    handler _ _ bsrc = return $ bsrc 

    

 

