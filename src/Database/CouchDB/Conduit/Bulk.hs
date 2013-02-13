{-# LANGUAGE OverloadedStrings #-}

-- | wrapper for CouchDB's bulk API see
-- <http://wiki.apache.org/couchdb/HTTP_Bulk_Document_API>

module Database.CouchDB.Conduit.Bulk 
    ( couchPostBulk
    , couchPostBulk'
    ) where

import Control.Monad (void)

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.LowLevel

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Data.Aeson (ToJSON, encode, object, (.=))



-- | \"Don\'t care\" version of 'couchPostBulk'
couchPostBulk' :: (MonadCouch m, ToJSON a)
               => Path -- ^Database
               -> [a]  -- ^Documents
               -> m ()
couchPostBulk' db = void . couchPostBulk db

-- | posts documents using CouchDB's bulk API
couchPostBulk :: (MonadCouch m, ToJSON a)
               => Path -- ^Database
               -> [a]  -- ^Documents
               -> m (CouchResponse m)
couchPostBulk db docs =
    couch HT.methodPost 
              (mkPath [db,"_bulk_docs"])
              [(HT.hContentType, "application/json")] [] 
              (H.RequestBodyLBS $ encode $ object ["docs" .= docs])
              protect'
