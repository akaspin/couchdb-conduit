{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Test.Util where

import Data.ByteString

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB

setupDB :: ByteString -> IO ()
setupDB n = runCouch "localhost" 5984 n $ couchPutDB ""

tearDB :: ByteString -> IO ()
tearDB n = runCouch "localhost" 5984 n $ couchDeleteDB ""
