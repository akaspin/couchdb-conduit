{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Test.Util where

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString

import Data.Conduit (runResourceT)
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB

import CouchDBAuth
import Control.Monad.Base (liftBase)

--setupDB :: ByteString -> IO ()
setupDB n = runCouch conn $ couchPutDB_ n

--tearDB :: ByteString -> IO ()
tearDB n = runCouch conn $ couchDeleteDB n

-- | Connection connection. See readme
conn :: CouchConnection
conn = def {
    couchLogin = login,
    couchPass = pass,
    couchPrefix = "cct__"}
