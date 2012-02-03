{- | CouchDB 

To work with concrete objects, use the following modules:
  
  * "Database.CouchDB.Conduit.DB" Database
  
  * "Database.CouchDB.Conduit.View" Views
  
  * "Database.CouchDB.Conduit.LowLevel" Low-level methods
   
For complete documentation about The Couch DB HTTP API see
<http://wiki.apache.org/couchdb/Complete_HTTP_API_Reference>
-}

module Database.CouchDB.Conduit (
    -- * Document paths and revisions #path#
    Path,
    Revision,
    
    -- * CouchDB Connection #connection#
    CouchConnection,
    def,
    couchHost,
    couchPort,
    couchManager,
    couchLogin,
    couchPass,
    
    -- * Runtime enviroment and errors #runtime#
    MonadCouch (..),
    CouchError (..),
    runCouch,
    withCouchConnection,
    
    -- * Documents
    --   
    --   For accessing and storing document data, use one of following: 
    --
    --   * "Database.CouchDB.Conduit.Explicit" Explicit JSON methods
    --   
    --   * "Database.CouchDB.Conduit.Generic" Generic JSON methods
    couchRev, 
    couchRev',
    couchDelete
    
) where

import Data.Conduit (ResourceT)
import Database.CouchDB.Conduit.Internal.Connection
import qualified Database.CouchDB.Conduit.Internal.Doc as D

-- | Get Revision of a document.
couchRev :: MonadCouch m => 
       Path       -- ^ Database.
    -> Path       -- ^ Document path.
    -> ResourceT m Revision
couchRev db p = D.couchRev (mkPath [db, p]) 

-- | Brain-free version of 'couchRev'. If document absent, 
--   just return 'B.empty'.
couchRev' :: MonadCouch m => 
       Path       -- ^ Database.
    -> Path       -- ^ Document path.
    -> ResourceT m Revision
couchRev' db p = D.couchRev' (mkPath [db, p]) 

-- | Delete the given revision of the object.  
couchDelete :: MonadCouch m => 
       Path       -- ^ Database.
    -> Path       -- ^ Document path.
    -> Revision             -- ^ Revision
    -> ResourceT m ()
couchDelete db p = D.couchDelete (mkPath [db, p]) 

