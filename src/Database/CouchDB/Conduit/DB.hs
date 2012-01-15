{- | CouchDB database methods.

/Note about paths./ If you passed a database name to 
"Database.CouchDB.Conduit#connection", the path in methods below should be the 
'B.empty'. But, if you passed the 'B.empty' to 'CouchConnection', then the 
@couchDB@ should be used in these methods.

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
import Database.CouchDB.Conduit.LowLevel (couch, protect, protect')

-- | Create CouchDB database.
couchPutDB :: MonadCouch m =>
       Path     -- ^ CouchDB Database name. See note above. 
    -> ResourceT m ()
couchPutDB p = couch HT.methodPut p [] []
                    (H.RequestBodyBS B.empty) protect'
                    >> return ()

-- | \"Don't care\" version of couchPutDb. Create CouchDB database only in its 
--   absence. Catches 'CouchError' @412@.
couchPutDB_ :: MonadCouch m =>
       Path     -- ^ CouchDB Database name. See note above. 
    -> ResourceT m ()
couchPutDB_ p = 
    couch HT.methodPut p [] []
                    (H.RequestBodyBS B.empty) 
                    (protect [200, 201, 202, 304] return) 
                    >> return ()
--    catch (couchPutDB p) handler
--  where
--    handler (CouchError (Just 412) _) = return ()
--    handler e = lift $ resourceThrow e

-- | Delete a database.
couchDeleteDB :: MonadCouch m => 
       Path     -- ^ CouchDB Database name. See note above. 
    -> ResourceT m ()
couchDeleteDB p = couch HT.methodDelete p [] []
                    (H.RequestBodyBS B.empty) protect'
                    >> return ()
