{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Test.Util where

import Data.ByteString

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB

setupDB :: ByteString -> IO ()
setupDB n = runCouch def {couchDB = n} $ couchPutDB ""

tearDB :: ByteString -> IO ()
tearDB n = runCouch  def {couchDB = n} $ couchDeleteDB ""
