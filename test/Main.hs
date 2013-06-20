
module Main (main) where

import Test.Framework (defaultMain)

import qualified Database.CouchDB.Conduit.Test.Base
import qualified Database.CouchDB.Conduit.Test.Bulk
import qualified Database.CouchDB.Conduit.Test.Explicit
import qualified Database.CouchDB.Conduit.Test.Generic
import qualified Database.CouchDB.Conduit.Test.View
import qualified Database.CouchDB.Conduit.Test.Query

main :: IO ()
main = defaultMain [
    Database.CouchDB.Conduit.Test.Base.tests,
    Database.CouchDB.Conduit.Test.Bulk.tests,
    Database.CouchDB.Conduit.Test.Explicit.tests,
    Database.CouchDB.Conduit.Test.Generic.tests,
    Database.CouchDB.Conduit.Test.View.tests,
    Database.CouchDB.Conduit.Test.Query.tests
    ]

