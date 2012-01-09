{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | Explicit methods for CouchDB documents. 
--   
--   See "Data.Aeson" for details.
module Database.CouchDB.Conduit.Explicit (
    -- * Accessing documents
    couchGet,
    couchRev,
    -- * Manipulating documents
    couchPut,
    couchPut',
    couchDelete,
    -- * Working with views #view#
    toType
) where

import qualified    Data.Aeson as A (FromJSON(..), ToJSON(..), Value(..),
                       fromJSON, encode)
import              Data.Conduit (Conduit(..), ResourceIO)

import              Network.HTTP.Types as HT (Query)

import              Database.CouchDB.Conduit (MonadCouch(..), Path, Revision)
import qualified    Database.CouchDB.Conduit.View ()
import              Database.CouchDB.Conduit.Internal.Doc (couchRev,
                        couchDelete, couchGetWith, couchPutWith, couchPutWith')
import              Database.CouchDB.Conduit.Internal.View

------------------------------------------------------------------------------
-- Document
------------------------------------------------------------------------------

-- | Load a single object with 'Revision' from couch DB. 
couchGet :: (MonadCouch m, A.FromJSON a) => 
       Path         -- ^ Document path
    -> HT.Query     -- ^ Query
    -> m (Revision, a)
couchGet = couchGetWith A.fromJSON

-- | Put an object in Couch DB with revision, returning the new 'Revision'.
couchPut :: (MonadCouch m, A.ToJSON a) => 
        Path        -- ^ Document path.
     -> Revision    -- ^ Document revision. For new docs provide empty string.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut = couchPutWith A.encode

-- | Brute force version of 'couchPut'.
couchPut' :: (MonadCouch m, A.ToJSON a) => 
        Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut' = couchPutWith' A.encode

------------------------------------------------------------------------------
-- View conduit
------------------------------------------------------------------------------

-- | Convert CouchDB view row or row value from "Database.CouchDB.Conduit.View" 
--   to concrete type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toType :: (ResourceIO m, A.FromJSON a) => Conduit A.Value m a
toType = toTypeWith A.fromJSON 



