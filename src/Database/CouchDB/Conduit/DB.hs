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
    couchPutDB',
    couchDeleteDB
) where

import Prelude hiding (catch)
import Control.Exception.Lifted (catch)

import qualified Data.ByteString as B

import Data.Conduit (runResourceT, resourceThrow)

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit (MonadCouch(..), CouchError(..), Path)
import Database.CouchDB.Conduit.LowLevel (couch, protect')

-- | Create CouchDB database.
couchPutDB :: MonadCouch m =>
       Path     -- ^ CouchDB Database name. See note above. 
    -> m ()
couchPutDB p = runResourceT $ couch HT.methodPut p [] []
                    (H.RequestBodyBS B.empty) protect'
                    >> return ()

-- | Brute force version of couchPutDb. Create CouchDB database regardless 
--   of presence. Catches 'CouchError' @412@.
couchPutDB' :: MonadCouch m =>
       Path     -- ^ CouchDB Database name. See note above. 
    -> m ()
couchPutDB' p = 
    catch (couchPutDB p) handler
  where
    handler (CouchError (Just 412) _) = return ()
    handler e = resourceThrow e

-- | Delete a database.
couchDeleteDB :: MonadCouch m => 
       Path     -- ^ CouchDB Database name. See note above. 
    -> m ()
couchDeleteDB p = runResourceT $ couch HT.methodDelete p [] []
                    (H.RequestBodyBS B.empty) protect'
                    >> return ()
