{-# LANGUAGE OverloadedStrings #-}

-- | Explicit methods for CouchDB documents.
module Database.CouchDB.Conduit.Explicit (
    couchRev,
    couchGet,
    couchPut,
    couchDelete
) where

import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import qualified Data.Aeson as A

import Data.Conduit (runResourceT, resourceThrow)

import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types as HT

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Internal

-- | Get Revision of a document. 
couchRev :: MonadCouch m => 
       DocPath 
    -> m Revision
couchRev p = runResourceT $ couch HT.methodHead p [] [] 
            (protect sinkRev) 
            (H.RequestBodyBS B.empty)
  where
    sinkRev _s h _bsrc = return $ B.tail . B.init . fromJust $ lookup "Etag" h

-- | Load a single object from couch DB.
couchGet :: (MonadCouch m, A.FromJSON a) => 
       DocPath      -- ^ Document path
    -> HT.Query     -- ^ Query
    -> m a
couchGet p q = do
    res <- runResourceT $ couch HT.methodGet p [] q 
            (protect sinkJSON) 
            (H.RequestBodyBS B.empty)
    case A.fromJSON res of
        A.Error e -> resourceThrow $ CouchError Nothing 
                        ("Error parsing json: " ++ e)
        A.Success o -> return o

-- | Put an object in Couch DB with revision, returning the new Revision.
couchPut :: (MonadCouch m, A.ToJSON a) => 
        DocPath     -- ^ Document path.
     -> Revision    -- ^ Document revision. For new docs provide empty string.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut p r q val = do
    res <- runResourceT $ couch HT.methodPut p (ifMatch r) q 
            (protect sinkJSON)
            (H.RequestBodyLBS $ A.encode val)
    either resourceThrow return (valToRev res)
  where 
    ifMatch "" = []
    ifMatch rv = [("If-Match", rv)]

-- | Delete the given revision of the object.    
couchDelete :: MonadCouch m => 
       DocPath 
    -> Revision
    -> m ()
couchDelete p r = runResourceT $ couch HT.methodDelete p 
               [] [("rev", Just r)]
               (protect sinkZero) 
               (H.RequestBodyBS B.empty)
 

