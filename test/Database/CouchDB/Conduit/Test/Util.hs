{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Test.Util where

import Data.ByteString

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB

import CouchDBAuth

setupDB :: ByteString -> IO ()
setupDB n = runCouch (conn n) $ couchPutDB_

tearDB :: ByteString -> IO ()
tearDB n = runCouch (conn n) $ couchDeleteDB

-- | Connection connection. See readme
conn :: Path -> CouchConnection
conn db = def {
    couchDB = db, 
    couchLogin = login,
    couchPass = pass}
