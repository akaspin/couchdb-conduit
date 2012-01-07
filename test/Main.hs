
module Main (main) where

import Test.Framework (defaultMain)

import qualified Database.CouchDB.Conduit.Test.Explicit
import qualified Database.CouchDB.Conduit.Test.View

main :: IO ()
main = defaultMain [
--      Database.CouchDB.Conduit.Test.Explicit.tests,
     Database.CouchDB.Conduit.Test.View.tests
    ]

