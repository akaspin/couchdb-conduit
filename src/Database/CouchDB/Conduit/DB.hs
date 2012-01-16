{- | CouchDB database methods.

> runCouch def $ couchPutDb "my_new_db"
> runCouch def {couchDB="another_new_db"} $ couchPutDb ""
-}

module Database.CouchDB.Conduit.DB (
    -- * Methods
    couchPutDB,
    couchPutDB_,
    couchDeleteDB
) where

import qualified Data.ByteString as B

import Data.Conduit (ResourceT)

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit (MonadCouch(..), Path)
import Database.CouchDB.Conduit.LowLevel

-- | Create CouchDB database. 
couchPutDB :: MonadCouch m =>
       Path     -- ^ CouchDB Database name.
    -> ResourceT m ()
couchPutDB p = couch HT.methodPut (const p) [] []
                    (H.RequestBodyBS B.empty) protect' 
                    >> return ()

-- | \"Don't care\" version of couchPutDb. Create CouchDB database only in its 
--   absence. For this it handles @412@ responses.
couchPutDB_ :: MonadCouch m =>
       Path     -- ^ CouchDB Database name.
    -> ResourceT m ()
couchPutDB_ p = 
    couch HT.methodPut (const p) [] []
                    (H.RequestBodyBS B.empty) 
                    (protect [200, 201, 202, 304, 412] return) 
                    >> return ()

-- | Delete a database.
couchDeleteDB :: MonadCouch m => 
       Path     -- ^ CouchDB Database name.
    -> ResourceT m ()
couchDeleteDB p = couch HT.methodDelete (const p) [] []
                    (H.RequestBodyBS B.empty) protect' 
                    >> return ()

-- | Database replication. 
--
--   See <http://guide.couchdb.org/editions/1/en/api.html#replication> for 
--   details.
couchReplicateDB :: MonadCouch m => 
       B.ByteString     -- ^ Source database. Path or URL 
    -> B.ByteString     -- ^ Target database. Path or URL 
    -> Bool             -- ^ Target creation flag
    -> Bool             -- ^ Continuous flag
    -> Bool             -- ^ Cancel flag
    -> ResourceT m ()
couchReplicateDB source target createTarget continuous cancel = 
    undefined