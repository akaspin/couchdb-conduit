-- | Database methods
module Database.CouchDB.Conduit.DB (
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

import Database.CouchDB.Conduit

-- | Create CouchDB database.
couchPutDB :: MonadCouch m =>
       Path     -- ^ If you passed a database name to 'withCouchConnection',
                --   'runCouch', or 'CouchConnection', the path should be
                --   the empty string.  If you passed the empty string to
                --   'CouchConnection', then the dbname should be used here.
    -> m ()
couchPutDB p = runResourceT $ couch HT.methodPut p [] []
                    (H.RequestBodyBS B.empty) protect'
                    >> return ()

-- | Brute force version of couchPutDb. Create CouchDB database regardless 
--   of presence. Catches 'CouchError' /412/.
couchPutDB' :: MonadCouch m =>
       Path     -- ^ If you passed a database name to 'withCouchConnection',
                --   'runCouch', or 'CouchConnection', the path should be
                --   the empty string.  If you passed the empty string to
                --   'CouchConnection', then the dbname should be used here.
    -> m ()
couchPutDB' p = 
    catch (couchPutDB p) handler
  where
    handler (CouchError (Just 412) _) = return ()
    handler e = resourceThrow e

-- | Delete a database.
couchDeleteDB :: MonadCouch m => 
       Path     -- ^ If you passed a database name to 'withCouchConnection',
                --   'runCouch', or 'CouchConnection', the path should be
                --   the empty string.  If you passed the empty string to
                --   'CouchConnection', then the dbname should be used here.
    -> m ()
couchDeleteDB p = runResourceT $ couch HT.methodDelete p [] []
                    (H.RequestBodyBS B.empty) protect'
                    >> return ()
