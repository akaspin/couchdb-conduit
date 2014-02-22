{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE OverloadedStrings #-} 

{- | CouchDB database methods.

> runCouch def {couchDB="my_db"} $ couchPutDb
> runCouch def {couchDB="my_new_db"} $ couchPutDb
-}

module Database.CouchDB.Conduit.DB (
    -- * Methods
    couchPutDB,
    couchPutDB_,
    couchDeleteDB,
    -- * Security
    couchSecureDB,
    -- * Replication
    couchReplicateDB
) where

import Control.Monad (void)

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit.Internal.Connection 
            (MonadCouch(..), Path, mkPath)
import Database.CouchDB.Conduit.LowLevel (couch, couch', protect, protect')


-- | Create CouchDB database. 
couchPutDB :: MonadCouch m => 
       Path             -- ^ Database
    -> m ()
couchPutDB db = void $ couch HT.methodPut 
                            (mkPath [db]) [] [] 
                            (H.RequestBodyBS B.empty)
                            protect'

-- | \"Don't care\" version of couchPutDb. Create CouchDB database only in its 
--   absence. For this it handles @412@ responses.
couchPutDB_ :: MonadCouch m => 
       Path             -- ^ Database
    -> m ()
couchPutDB_ db = void $ couch HT.methodPut 
                    (mkPath [db]) [] []
                    (H.RequestBodyBS B.empty) 
                    (protect [200, 201, 202, 304, 412] return) 

-- | Delete a database.
couchDeleteDB :: MonadCouch m => 
       Path             -- ^ Database
    -> m ()
couchDeleteDB db = void $ couch HT.methodDelete 
                    (mkPath [db]) [] []
                    (H.RequestBodyBS B.empty) protect' 

-- | Maintain DB security.
couchSecureDB :: MonadCouch m => 
       Path             -- ^ Database
    -> [B.ByteString]   -- ^ Admin roles 
    -> [B.ByteString]   -- ^ Admin names
    -> [B.ByteString]   -- ^ Readers roles 
    -> [B.ByteString]   -- ^ Readers names
    -> m ()       
couchSecureDB db adminRoles adminNames readersRoles readersNames = 
    void $ couch HT.methodPut 
            (mkPath [db, "_security"]) [] []
            reqBody protect' 
  where
    reqBody = H.RequestBodyLBS $ A.encode $ A.object [
            "admins" A..= A.object [ "roles" A..= map TE.decodeUtf8 adminRoles,
                                     "names" A..= map TE.decodeUtf8 adminNames ],
            "readers" A..= A.object [ "roles" A..= map TE.decodeUtf8 readersRoles,
                                      "names" A..= map TE.decodeUtf8 readersNames ] ]

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
    -> m ()
couchReplicateDB source target createTarget continuous cancel = 
    void $ couch' HT.methodPost (const "/_replicate") [] []
            reqBody protect' 
  where
    reqBody = H.RequestBodyLBS $ A.encode $ A.object [
            "source" A..= TE.decodeUtf8 source,
            "target" A..= TE.decodeUtf8 target,
            "create_target" A..= createTarget,
            "continuous" A..= continuous,
            "cancel" A..= cancel ]

        
        
        
        
        
        