
module Main (main) where

import Test.Framework (defaultMain, Test)

import qualified Database.CouchDB.Conduit.Test.Basic

main :: IO ()
main = defaultMain tests

-- | All tests
tests :: [Test]
tests = [
        Database.CouchDB.Conduit.Test.Basic.tests
        
    ]
