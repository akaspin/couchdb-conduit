{-# LANGUAGE OverloadedStrings #-}

-- | Internal
module Database.CouchDB.Conduit.Internal.Doc (
    couchRev,
    couchDelete,
    
    couchGetWith,
    couchPutWith,
    couchPutWith_,
    couchPutWith',
    
    couchGetRaw
) where

import              Prelude hiding (catch)
import              Control.Exception.Lifted (catch)
import              Control.Monad.Trans.Class (lift)
import              Data.Maybe (fromJust)
import qualified    Data.ByteString as B
import qualified    Data.ByteString.Lazy as BL
import qualified    Data.Text.Encoding as TE
import qualified    Data.Aeson as A
import              Data.Conduit (ResourceT, resourceThrow, ($$))
import qualified    Data.Conduit.Attoparsec as CA
import qualified    Network.HTTP.Conduit as H
import              Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import              Database.CouchDB.Conduit.LowLevel 
                        (couch, protect')
import              Database.CouchDB.Conduit.Internal.Parser

------------------------------------------------------------------------------
-- Type-independent methods
------------------------------------------------------------------------------

-- | Get Revision of a document. 
couchRev :: MonadCouch m => 
       Path 
    -> ResourceT m Revision
couchRev p = do
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
    -> ResourceT m ()
couchDelete p r = couch HT.methodDelete p 
               [] [("rev", Just r)]
               (H.RequestBodyBS B.empty)
               protect' >> return ()
               
------------------------------------------------------------------------------
-- with converter
------------------------------------------------------------------------------

-- | Load CouchDB document and parse it with given parser   
couchGetWith :: MonadCouch m =>
          (A.Value -> A.Result a)       -- ^ Parser
       -> Path                          -- ^ Path
       -> Query                         -- ^ Query
       -> ResourceT m (Revision, a)
couchGetWith f p q = do
    H.Response _ _ bsrc <- couch HT.methodGet p [] q 
            (H.RequestBodyBS B.empty) protect'
    j <- bsrc $$ CA.sinkParser A.json
    A.String r <- lift $ either resourceThrow return $ extractField "_rev" j
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
       -> ResourceT m Revision
couchPutWith f p r q val = do
    H.Response _ _ bsrc <- couch HT.methodPut p (ifMatch r) q 
            (H.RequestBodyLBS $ f val) protect'
    j <- bsrc $$ CA.sinkParser A.json
    lift $ either resourceThrow return $ extractRev j
  where 
    ifMatch "" = []
    ifMatch rv = [("If-Match", rv)]
    
-- | \"Don't care\"  version of version of 'couchPutWith'. Stores 
--   document only if it not exists.
couchPutWith_ :: MonadCouch m => 
        (a -> BL.ByteString)  -- ^ Encoder
     -> Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> ResourceT m Revision      
couchPutWith_ f p q val = do
    rev <- catch (couchRev p) handler404
    if rev == "" then couchPutWith f p "" q val
        else return ""
  where 
    handler404 (CouchError (Just 404) _) = return ""
    handler404 e = lift $ resourceThrow e

-- | Brute force version of 'couchPutWith'.
couchPutWith' :: MonadCouch m => 
        (a -> BL.ByteString)  -- ^ Encoder
     -> Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> ResourceT m Revision      
couchPutWith' f p q val = do
    rev <- catch (couchRev p) handler404
    couchPutWith f p rev q val
  where 
    handler404 (CouchError (Just 404) _) = return ""
    handler404 e = lift $ resourceThrow e


-- | Load raw 'A.Value' from single object from couch DB.
couchGetRaw :: MonadCouch m => 
       Path         -- ^ Document path
    -> HT.Query     -- ^ Query
    -> ResourceT m (Revision, A.Value)
couchGetRaw p q = do
    H.Response _ _ bsrc <- couch HT.methodGet p [] q 
            (H.RequestBodyBS B.empty) protect'
    j <- bsrc $$ CA.sinkParser A.json
    A.String r <- lift $ either resourceThrow return $ extractField "_rev" j
    return (TE.encodeUtf8 r, j)