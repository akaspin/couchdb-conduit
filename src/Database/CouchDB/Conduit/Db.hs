
module Database.CouchDB.Conduit.Db (
    couchPutDb,
    couchPutDb',
    couchDeleteDb
) where

import Prelude hiding (catch)

import Control.Exception.Lifted (catch)

import qualified Data.ByteString as B

import Data.Conduit (runResourceT, resourceThrow)

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Internal

-- | Create CouchDB database.
couchPutDb :: MonadCouch m =>
       DbPath   -- ^ If you passed a database name to 'withCouchConnection',
                --   'runCouch', or 'CouchConnection', the path should be
                --   the empty string.  If you passed the empty string to
                --   'CouchConnection', then the dbname should be used here.
    -> m ()
couchPutDb p = runResourceT $ couch HT.methodPut p [] []
                    (protect sinkZero)
                    (H.RequestBodyBS B.empty)

-- | Brute force version of couchPutDb. Create CouchDB database regardless 
--   of presence. Catches 'CouchError' /412/.
couchPutDb' :: MonadCouch m =>
       DbPath   -- ^ If you passed a database name to 'withCouchConnection',
                --   'runCouch', or 'CouchConnection', the path should be
                --   the empty string.  If you passed the empty string to
                --   'CouchConnection', then the dbname should be used here.
    -> m ()
couchPutDb' p = 
    catch (couchPutDb p) handler
  where
    handler (CouchError (Just 412) _) = return ()
    handler e = resourceThrow e

-- | Delete a database.
couchDeleteDb :: MonadCouch m => 
       DbPath   -- ^ If you passed a database name to 'withCouchConnection',
                --   'runCouch', or 'CouchConnection', the path should be
                --   the empty string.  If you passed the empty string to
                --   'CouchConnection', then the dbname should be used here.
    -> m ()
couchDeleteDb p = runResourceT $ couch HT.methodDelete p [] []
                    (protect sinkZero)
                    (H.RequestBodyBS B.empty)