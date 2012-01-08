{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | Generic methods for CouchDB documents
module Database.CouchDB.Conduit.Generic (
    couchGet,
    couchPut,
    couchPut',
    couchRev,
    couchDelete,
    -- * Views
    toType,
) where

import              Data.Generics (Data)
import qualified    Data.Aeson as A
import qualified    Data.Aeson.Generic as AG
import              Data.Conduit (Conduit(..), ResourceIO)

import qualified    Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import              Database.CouchDB.Conduit.Internal.Doc
import              Database.CouchDB.Conduit.Internal.View

-- | Load a single object from couch DB.
couchGet :: (MonadCouch m, Data a) => 
       Path         -- ^ Document path
    -> HT.Query     -- ^ Query
    -> m (Revision, a)
couchGet = couchGetWith AG.fromJSON  

-- | Put an object in Couch DB with revision, returning the new Revision.
couchPut :: (MonadCouch m, Data a) => 
        Path        -- ^ Document path.
     -> Revision    -- ^ Document revision. For new docs provide empty string.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut = couchPutWith AG.encode
    
-- | Brute force version of 'couchPut'.
couchPut' :: (MonadCouch m, Data a) => 
        Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut' = couchPutWith' AG.encode

------------------------------------------------------------------------------
-- View conduit
------------------------------------------------------------------------------

-- | Convert CouchDB view row or row value from 'Database.CouchDB.Conduit.View' 
--   to concrete type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toType :: (ResourceIO m, Data a) => Conduit A.Value m a
toType = toTypeWith AG.fromJSON 