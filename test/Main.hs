
module Main (main) where

import Test.Framework (defaultMain)

import qualified Database.CouchDB.Conduit.Test.Explicit

main :: IO ()
main = defaultMain [
    Database.CouchDB.Conduit.Test.Explicit.tests
    ]

