
module Main (main) where

import Test.Framework (defaultMain, Test)
import qualified Database.CouchDB.Conduit.Mock.View 

main :: IO ()
main = defaultMain tests

-- | All tests
tests :: [Test]
tests = [
    Database.CouchDB.Conduit.Mock.View.tests
    ]
