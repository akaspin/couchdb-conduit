{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | Generic methods for CouchDB documents
module Database.CouchDB.Conduit.Generic (
    couchGet,
    couchPut,
    couchPut',
    couchRev,
    couchDelete,
    -- * Low-level
    couchGetRaw
) where

import              Prelude hiding (catch)

import              Control.Exception.Lifted (catch)
import              Control.Monad.Trans.Class (lift)

import              Data.ByteString as B
import qualified    Data.Text.Encoding as TE
import              Data.Generics (Data)
import qualified    Data.Aeson as A
import qualified    Data.Aeson.Generic as AG
import              Data.Conduit (runResourceT, resourceThrow, ($$))
import              Data.Conduit.Attoparsec as CA

import qualified    Network.HTTP.Conduit as H
import qualified    Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import              Database.CouchDB.Conduit.Internal.Doc
import              Database.CouchDB.Conduit.Internal.Parser

-- | Load a single object from couch DB.
couchGet :: (MonadCouch m, Data a) => 
       DocPath      -- ^ Document path
    -> HT.Query     -- ^ Query
    -> m (Revision, a)
couchGet p q = runResourceT $ do
    H.Response _ _ bsrc <- couch HT.methodGet p [] q 
            (H.RequestBodyBS B.empty) protect'
    j <- bsrc $$ CA.sinkParser A.json
    A.String r <- lift $ extractField "_rev" j
    obj <- parseObjToGen $ AG.fromJSON j
    return (TE.encodeUtf8 r, obj)
  where
    parseObjToGen (A.Error e) = lift $ resourceThrow $ CouchError Nothing 
                        ("Error parsing json: " ++ e)
    parseObjToGen (A.Success o) = return o
        
-- | Put an object in Couch DB with revision, returning the new Revision.
couchPut :: (MonadCouch m, Data a) => 
        DocPath     -- ^ Document path.
     -> Revision    -- ^ Document revision. For new docs provide empty string.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut p r q val = runResourceT $  do
    H.Response _ _ bsrc <- couch HT.methodPut p (ifMatch r) q 
            (H.RequestBodyLBS $ AG.encode val) protect'
    j <- bsrc $$ CA.sinkParser A.json
    lift $ extractRev j
  where 
    ifMatch "" = []
    ifMatch rv = [("If-Match", rv)]
    
-- | Brute force version of 'couchPut'.
couchPut' :: (MonadCouch m, Data a) => 
        DocPath     -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut' p q val = do
    rev <- catch (couchRev p) handler404
    couchPut p rev q val
  where 
    handler404 (CouchError (Just 404) _) = return ""
    handler404 e = resourceThrow e
