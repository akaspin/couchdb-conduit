{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | Explicit methods for CouchDB documents.
--   
--   See 'Data.Aeson' for details.
module Database.CouchDB.Conduit.Explicit (
    -- * document methods
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

import qualified    Data.ByteString as B
import qualified    Data.Aeson as A
import              Data.Conduit (runResourceT, resourceThrow, ($$))
import qualified    Data.Conduit.Attoparsec as CA

import qualified    Network.HTTP.Conduit as H
import              Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import              Database.CouchDB.Conduit.Internal.Doc
import              Database.CouchDB.Conduit.Internal.Parser

-- | Load a single object from couch DB.
couchGet :: (MonadCouch m, A.FromJSON a) => 
       DocPath      -- ^ Document path
    -> HT.Query     -- ^ Query
    -> m a
couchGet p q = runResourceT $ do
    H.Response _ _ bsrc <- couch HT.methodGet p [] q 
            (H.RequestBodyBS B.empty) protect'
    j <- bsrc $$ CA.sinkParser A.json
    case A.fromJSON j of
        A.Error e -> lift $ resourceThrow $ CouchError Nothing 
                        ("Error parsing json: " ++ e)
        A.Success o -> return o

-- | Put an object in Couch DB with revision, returning the new Revision.
couchPut :: (MonadCouch m, A.ToJSON a) => 
        DocPath     -- ^ Document path.
     -> Revision    -- ^ Document revision. For new docs provide empty string.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut p r q val = runResourceT $ do
    H.Response _ _ bsrc <- couch HT.methodPut p (ifMatch r) q 
            (H.RequestBodyLBS $ A.encode val) protect'
    j <- bsrc $$ CA.sinkParser A.json
    lift $ extractRev j
  where 
    ifMatch "" = []
    ifMatch rv = [("If-Match", rv)]

-- | Brute force version of 'couchPut'.
couchPut' :: (MonadCouch m, A.ToJSON a) => 
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


