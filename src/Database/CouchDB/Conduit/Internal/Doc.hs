{-# LANGUAGE OverloadedStrings #-}

-- | Internal
module Database.CouchDB.Conduit.Internal.Doc (
    couchRev,
    couchDelete,
    couchGetRaw,
    
    couchGetWith,
    couchPutWith,
    couchPutWith'
) where

import              Prelude hiding (catch)
import              Control.Exception.Lifted (catch)
import              Control.Monad.Trans.Class (lift)
import              Data.Maybe (fromJust)
import qualified    Data.ByteString as B
import qualified    Data.ByteString.Lazy as BL
import qualified    Data.Text.Encoding as TE
import qualified    Data.Aeson as A
import              Data.Conduit (runResourceT, resourceThrow, ($$))
import qualified    Data.Conduit.Attoparsec as CA
import qualified    Network.HTTP.Conduit as H
import              Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import              Database.CouchDB.Conduit.LowLevel (couch, protect')
import              Database.CouchDB.Conduit.Internal.Parser

------------------------------------------------------------------------------
-- Type-independent methods
------------------------------------------------------------------------------

-- | Get Revision of a document. 
couchRev :: MonadCouch m => 
       Path 
    -> m Revision
couchRev p = runResourceT $ do
    (H.Response _ hs _) <- couch HT.methodHead p [] [] 
            (H.RequestBodyBS B.empty)
            protect'
    return $ peekRev hs        
  where
    peekRev = B.tail . B.init . fromJust . lookup "Etag"

-- | Delete the given revision of the object.    
couchDelete :: MonadCouch m => 
       Path 
    -> Revision
    -> m ()
couchDelete p r = runResourceT $ couch HT.methodDelete p 
               [] [("rev", Just r)]
               (H.RequestBodyBS B.empty)
               protect' >> return ()
               
------------------------------------------------------------------------------
-- low-level 
------------------------------------------------------------------------------

-- | Load raw 'A.Value' from single object from couch DB.
couchGetRaw :: MonadCouch m => 
       Path         -- ^ Document path
    -> HT.Query     -- ^ Query
    -> m A.Value
couchGetRaw p q = runResourceT $ do
    H.Response _ _ bsrc <- couch HT.methodGet p [] q 
            (H.RequestBodyBS B.empty) protect'
    bsrc $$ CA.sinkParser A.json
    
------------------------------------------------------------------------------
-- with converter
------------------------------------------------------------------------------

-- | Load CouchDB document and parse it with given parser   
couchGetWith :: MonadCouch m =>
          (A.Value -> A.Result a)       -- ^ Parser
       -> Path                          -- ^ Path
       -> Query                         -- ^ Query
       -> m (Revision, a)
couchGetWith f p q = runResourceT $ do
    H.Response _ _ bsrc <- couch HT.methodGet p [] q 
            (H.RequestBodyBS B.empty) protect'
    j <- bsrc $$ CA.sinkParser A.json
    A.String r <- lift $ extractField "_rev" j
    o <- lift $ jsonToTypeWith f j 
    return (TE.encodeUtf8 r, o)

-- | Put document, with given encoder
couchPutWith :: MonadCouch m =>
          (a -> BL.ByteString)  -- ^ Encoder
       -> Path                  -- ^ Document path. 
       -> Revision              -- ^ Document revision. For new docs provide 
                                -- ^ empty string.
       -> Query                 -- ^ Query arguments.
       -> a                     -- ^ The object to store.
       -> m Revision
couchPutWith f p r q val = runResourceT $ do
    H.Response _ _ bsrc <- couch HT.methodPut p (ifMatch r) q 
            (H.RequestBodyLBS $ f val) protect'
    j <- bsrc $$ CA.sinkParser A.json
    lift $ extractRev j
  where 
    ifMatch "" = []
    ifMatch rv = [("If-Match", rv)]
    
-- | Brute force version of 'couchPutWith'.
couchPutWith' :: MonadCouch m => 
        (a -> BL.ByteString)  -- ^ Encoder
     -> Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> m Revision      
couchPutWith' f p q val = do
    rev <- catch (couchRev p) handler404
    couchPutWith f p rev q val
  where 
    handler404 (CouchError (Just 404) _) = return ""
    handler404 e = resourceThrow e

