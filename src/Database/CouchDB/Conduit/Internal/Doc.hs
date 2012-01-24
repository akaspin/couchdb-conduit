{-# LANGUAGE OverloadedStrings #-}

-- | Internal
module Database.CouchDB.Conduit.Internal.Doc (
    couchRev,
    couchRev',
    couchDelete,
    
    couchGetWith,
    couchPutWith,
    couchPutWith_,
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
import              Data.Conduit (ResourceT, resourceThrow, ($$))
import qualified    Data.Conduit.Attoparsec as CA
import qualified    Network.HTTP.Conduit as H
import              Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import              Database.CouchDB.Conduit.LowLevel (couch')
import              Database.CouchDB.Conduit.Internal.Parser

------------------------------------------------------------------------------
-- Type-independent methods
------------------------------------------------------------------------------

-- | Get Revision of a document. 
couchRev :: MonadCouch m => 
       Path                 -- ^ Path
    -> ResourceT m Revision
couchRev p = do
    (H.Response _ hs _) <- couch' HT.methodHead p [] [] 
                                (H.RequestBodyBS B.empty)
    return $ peekRev hs        
  where
    peekRev = B.tail . B.init . fromJust . lookup "Etag"

-- | Brain-free version of 'couchRev'. If document absent, 
--   just return 'B.empty'.
couchRev' :: MonadCouch m =>
       Path 
    -> ResourceT m Revision
couchRev' p = 
    catch (couchRev p) handler404
  where
    handler404 (CouchHttpError 404 _) = return B.empty
    handler404 e = lift $ resourceThrow e

-- | Delete the given revision of the object.    
couchDelete :: MonadCouch m => 
       Path 
    -> Revision
    -> ResourceT m ()
couchDelete p r = couch' HT.methodDelete p [] [("rev", Just r)]
               (H.RequestBodyBS B.empty) 
               >> return ()
               
------------------------------------------------------------------------------
-- with converter
------------------------------------------------------------------------------

-- | Load CouchDB document and parse it with given parser.  
couchGetWith :: MonadCouch m =>
          (A.Value -> A.Result a)       -- ^ Parser
       -> Path                          -- ^ Path
       -> Query                         -- ^ Query
       -> ResourceT m (Revision, a)
couchGetWith f p q = do
    H.Response _ _ bsrc <- couch' HT.methodGet p [] q 
                            (H.RequestBodyBS B.empty)
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
    H.Response _ _ bsrc <- couch' HT.methodPut p (ifMatch r) q 
            (H.RequestBodyLBS $ f val)
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
    rev <- couchRev' p
    if rev == "" 
        then couchPutWith f p "" q val
        else return rev

-- | Brute force version of 'couchPutWith'.
couchPutWith' :: MonadCouch m => 
        (a -> BL.ByteString)  -- ^ Encoder
     -> Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> ResourceT m Revision      
couchPutWith' f p q val = do
    rev <- couchRev' p
    couchPutWith f p rev q val
















