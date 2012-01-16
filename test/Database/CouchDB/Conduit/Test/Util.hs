{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Test.Util where

import Data.ByteString

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB

setupDB :: ByteString -> IO ()
setupDB n = runCouch (conn n) $ couchPutDB_ n

tearDB :: ByteString -> IO ()
tearDB n = runCouch (conn n) $ couchDeleteDB n

-- | Connection connection. See readme
conn :: Path -> CouchConnection
conn db = def {
    couchDB = db, 
    couchLogin = "root",
    couchPass = "sumatra"}
