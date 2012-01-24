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

import qualified Data.ByteString as B
import qualified Data.Aeson as A

import Data.Conduit (ResourceT)

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit (MonadCouch(..))
import Database.CouchDB.Conduit.LowLevel (couch, protect, protect')


-- | Create CouchDB database. 
couchPutDB :: MonadCouch m => ResourceT m ()
couchPutDB = couch HT.methodPut id [] []
                    (H.RequestBodyBS B.empty) protect' 
                    >> return ()

-- | \"Don't care\" version of couchPutDb. Create CouchDB database only in its 
--   absence. For this it handles @412@ responses.
couchPutDB_ :: MonadCouch m => ResourceT m ()
couchPutDB_ = couch HT.methodPut id [] []
                    (H.RequestBodyBS B.empty) 
                    (protect [200, 201, 202, 304, 412] return) 
                    >> return ()

-- | Delete a database.
couchDeleteDB :: MonadCouch m => ResourceT m ()
couchDeleteDB = couch HT.methodDelete id [] []
                    (H.RequestBodyBS B.empty) protect' 
                    >> return ()

-- | Maintain DB security.
couchSecureDB :: MonadCouch m => 
       [B.ByteString]   -- ^ Admin roles 
    -> [B.ByteString]   -- ^ Admin names
    -> [B.ByteString]   -- ^ Readers roles 
    -> [B.ByteString]   -- ^ Readers names
    -> ResourceT m ()       
couchSecureDB adminRoles adminNames readersRoles readersNames = 
    couch HT.methodPut (`B.append` "/_security") [] []
            reqBody protect' 
            >> return ()
  where
    reqBody = H.RequestBodyLBS $ A.encode $ A.object [
            "admins" A..= A.object [ "roles" A..= adminRoles,
                                     "names" A..= adminNames ],
            "readers" A..= A.object [ "roles" A..= readersRoles,
                                     "names" A..= readersNames ] ]

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
    couch HT.methodPost (const "_replicate") [] []
            reqBody protect' 
            >> return ()
  where
    reqBody = H.RequestBodyLBS $ A.encode $ A.object [
            "source" A..= source,
            "target" A..= target,
            "create_target" A..= createTarget,
            "continuous" A..= continuous,
            "cancel" A..= cancel ]

        
        
        
        
        
        