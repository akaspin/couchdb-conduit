{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Test.Util where

import Data.Text

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB

import CouchDBAuth

setupDB :: Text -> IO ()
setupDB n = runCouch conn $ couchPutDB_ n

tearDB :: Text -> IO ()
tearDB n = runCouch conn $ couchDeleteDB n

-- | Connection connection. See readme
conn :: CouchConnection
conn = def {
    couchLogin = login,
    couchPass = pass,
    couchPrefix = "cct__"}
