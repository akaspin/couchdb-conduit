{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}

module Database.CouchDB.Conduit.Test.Connect (tests) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import Control.Monad.Base (liftBase)

import Data.Aeson as A
import Data.Conduit
import Data.Conduit.Attoparsec
import Network.HTTP.Conduit


tests :: Test
tests = testGroup "http-conduit" [
      testCase "Consumer" case_insideWith
    ]

case_insideWith :: Assertion
case_insideWith = do
    request <- liftBase $ parseUrl "http://localhost:5984/"
    withManager $ \man -> do
        res <- http request handlerJ man
        liftBase $ print res



--handlerJ _code _hdrs bsrc = bsrc $$ consume
handlerJ _code _hdrs bsrc = bsrc $$ sinkParser A.json
