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
    -- * Views
    toType,
    -- * Low-level
    couchGetRaw
) where

import              Prelude hiding (catch)

import              Control.Exception.Lifted (catch)
import              Control.Monad.Trans.Class (lift)

import qualified    Data.ByteString as B (empty)
import qualified    Data.Aeson as A (FromJSON(..), ToJSON(..), Value(..),
                        Result(..), fromJSON, encode, json)
import qualified    Data.Text.Encoding as TE (encodeUtf8)
import              Data.Conduit (runResourceT, resourceThrow, ($$), 
                        Conduit(..), ResourceIO)
import qualified    Data.Conduit.List as CL (mapM)
import qualified    Data.Conduit.Attoparsec as CA (sinkParser)

import qualified    Network.HTTP.Conduit as H (Response(..), RequestBody(..))
import              Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import qualified    Database.CouchDB.Conduit.View ()
import              Database.CouchDB.Conduit.Internal.Doc
import              Database.CouchDB.Conduit.Internal.Parser

------------------------------------------------------------------------------
-- Document
------------------------------------------------------------------------------

-- | Load a single object with 'Revision' from couch DB.
couchGet :: (MonadCouch m, A.FromJSON a) => 
       Path         -- ^ Document path
    -> HT.Query     -- ^ Query
    -> m (Revision, a)
couchGet p q = runResourceT $ do
    H.Response _ _ bsrc <- couch HT.methodGet p [] q 
            (H.RequestBodyBS B.empty) protect'
    j <- bsrc $$ CA.sinkParser A.json
    A.String r <- lift $ extractField "_rev" j
    case A.fromJSON j of
        A.Error e -> lift $ resourceThrow $ CouchError Nothing 
                        ("Error parsing json: " ++ e)
        A.Success o -> return (TE.encodeUtf8 r, o)

-- | Put an object in Couch DB with revision, returning the new Revision.
couchPut :: (MonadCouch m, A.ToJSON a) => 
        Path        -- ^ Document path.
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
        Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPut' p q val = do
    rev <- catch (couchRev p) handler404
    couchPut p rev q val
  where 
    handler404 (CouchError (Just 404) _) = return ""
    handler404 e = resourceThrow e

------------------------------------------------------------------------------
-- View conduit
------------------------------------------------------------------------------

-- | Convert CouchDB view row or row value from 'Database.CouchDB.Conduit.View' 
--   to concrete type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toType :: (ResourceIO m, A.FromJSON a) => Conduit A.Value m a
toType = CL.mapM (\v -> case A.fromJSON v of
            A.Error e -> resourceThrow $ CouchError Nothing 
                            ("Error parsing json: " ++ e)
            A.Success o -> return o)



