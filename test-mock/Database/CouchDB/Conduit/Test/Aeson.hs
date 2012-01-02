-- | tests for aeson
module Database.CouchDB.Conduit.Test.Aeson where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Control.Monad.Base (liftBase)

import Data.Conduit
import Network.HTTP.Conduit


tests :: Test
tests = testGroup "http-conduit" [
    testCase "just iterate" case_basic 
    ]
    
case_basic :: Assertion
case_basic = do
    request <- parseUrl "http://localhost:5984/"
    runResourceT $ do
        man <- newManager
        Response sc _ _ <- httpLbsRedirect request man
        liftBase $ sc @=? 200