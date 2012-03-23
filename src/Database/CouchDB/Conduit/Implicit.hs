-- | Implicit methods for CouchDB documents.
--
--   All implicit methods needs parser or encoder.

module Database.CouchDB.Conduit.Implicit (
     -- * Accessing documents
    couchGet,
    -- * Manipulating documents
    couchPut,
    couchPut_,
    couchPut',
) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL (ByteString)

import Database.CouchDB.Conduit.Internal.Connection 
        (MonadCouch(..), Path, mkPath, Revision)
import Database.CouchDB.Conduit.Internal.Doc (couchGetWith, couchPutWith, 
        couchPutWith_, couchPutWith')
        
import Network.HTTP.Types (Query)

-- | Load CouchDB document and parse it with given parser.
--
-- > (rev, rawJson) <- couchGet Success "mydb" mydoc []
couchGet :: MonadCouch m =>
       (A.Value -> A.Result a)    -- ^ Parser
    -> Path                       -- ^ Document path.
    -> Path                       -- ^ Document path.
    -> Query                      -- ^ Query
    -> m (Revision, a)
couchGet f db p = couchGetWith f (mkPath [db, p])

-- | Put document, with given encoder
couchPut :: MonadCouch m =>
      (a -> BL.ByteString)  -- ^ Encoder
   -> Path                  -- ^ Document path.
   -> Path                  -- ^ Document path.
   -> Revision              -- ^ Document revision. For new docs provide 
                            -- ^ empty string.
   -> Query                 -- ^ Query arguments.
   -> a                     -- ^ The object to store.
   -> m Revision
couchPut f db p = couchPutWith f (mkPath [db, p])

-- | \"Don't care\" version of 'couchPut'. Creates document only in its 
--   absence.
couchPut_ :: MonadCouch m =>
      (a -> BL.ByteString)  -- ^ Encoder
   -> Path                  -- ^ Document path.
   -> Path                  -- ^ Document path.
   -> Query                 -- ^ Query arguments.
   -> a                     -- ^ The object to store.
   -> m Revision
couchPut_ f db p = couchPutWith_ f (mkPath [db, p])

-- | Brute force version of 'couchPut'. Creates a document regardless of 
--   presence. 
couchPut' :: MonadCouch m =>
      (a -> BL.ByteString)  -- ^ Encoder
   -> Path                  -- ^ Document path.
   -> Path                  -- ^ Document path.
   -> Query                 -- ^ Query arguments.
   -> a                     -- ^ The object to store.
   -> m Revision
couchPut' f db p = couchPutWith' f (mkPath [db, p])

