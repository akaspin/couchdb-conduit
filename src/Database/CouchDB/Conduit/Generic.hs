{-# LANGUAGE OverloadedStrings #-}
-- | Generic methods for CouchDB documents
module Database.CouchDB.Conduit.Generic where

import Data.ByteString as B
import Data.Generics (Data)
import Data.Conduit (runResourceT, resourceThrow)
import qualified Data.Aeson as A
import qualified Data.Aeson.Generic as AG

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Internal

-- | Load a single object from couch DB.
couchGet :: (MonadCouch m, Data a) => 
       DocPath      -- ^ Document path
    -> HT.Query     -- ^ Query
    -> m (Revision, a)
couchGet p q = do
    res <- runResourceT $ couch HT.methodGet p [] q 
            (protect sinkJSON) 
            (H.RequestBodyBS B.empty)
    rev <- either resourceThrow return $ valToRev res
    case AG.fromJSON res of
        A.Error e -> resourceThrow $ CouchError Nothing 
                        ("Error parsing json: " ++ e)
        A.Success o -> return (rev, o)
        
-- | Put an object in Couch DB with revision, returning the new Revision.
couchPut :: (MonadCouch m, Data a) => 
        DocPath     -- ^ Document path.
     -> Revision    -- ^ Document revision. For new docs provide empty string.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut p r q val = do
    res <- runResourceT $ couch HT.methodPut p (ifMatch r) q 
            (protect sinkJSON)
            (H.RequestBodyLBS $ AG.encode val)
    either resourceThrow return (valToRev res)
  where 
    ifMatch "" = []
    ifMatch rv = [("If-Match", rv)]
