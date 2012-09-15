{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | Generic methods for CouchDB documents. Unlike explicit, generic methods 
--   uses "Data.Generics".
--
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- >
-- > import Control.Monad.IO.Class (liftIO)
-- > import Data.Generic (Data, Typeable)
-- > import Database.CouchDB.Conduit
-- > import Database.CouchDB.Conduit.Generic
-- >
-- > -- | Our doc with instances
-- > data D = D { f1 :: Int, f2 :: String } deriving (Show, Data, Typeable)
-- > 
-- > runCouch def $ do
-- >    -- Put new doc and update it
-- >    rev1 <- couchPut "mydb" "my-doc1" "" [] $ D 123 "str"         
-- >    rev2 <- couchPut "mydb" "my-doc1" rev1 [] $ D 1234 "another"
-- >
-- >    -- get it and print
-- >    (rev3, d1 :: D) <- couchGet "mydb" "my-doc1" [] 
-- >    liftIO $ print d1
-- >
-- >    -- update it in brute-force manner    
-- >    couchPut' "mydb" "my-doc1" [] $ D 12345 "third"    -- notice - no rev
-- >    
-- >    -- get revision and delete
-- >    rev3 <- couchRev "mydb" "my-doc1"
-- >    couchDelete "mydb" "my-doc1" rev3
--  
--   The main advantage of this approach in the absence of tonns of  
--   boilerplate code. The main disadvantage is inability to influence the 
--   process of translation to and from JSON.
-- 
--   For details of types see "Data.Aeson.Generic". To work with documents in 
--   explicit manner, look at "Database.CouchDB.Conduit.Explicit".

module Database.CouchDB.Conduit.Generic (
     -- * Accessing documents
    couchGet,
    -- * Manipulating documents
    couchPut,
    couchPut_,
    couchPut',
    -- * Working with views #view#
    toType
) where

import Data.Generics (Data)
import qualified    Data.Aeson as A
import qualified    Data.Aeson.Generic as AG
import Data.Conduit (Conduit, MonadResource)

import Network.HTTP.Types (Query)

import Database.CouchDB.Conduit.Internal.Connection 
            (MonadCouch(..), Path, mkPath, Revision)
import Database.CouchDB.Conduit.Internal.Doc 
            (couchGetWith, couchPutWith, couchPutWith_, couchPutWith')
import Database.CouchDB.Conduit.Internal.View (toTypeWith)

-- | Load a single object from couch DB.
couchGet :: (MonadCouch m, Data a) => 
       Path         -- ^ Database
    -> Path         -- ^ Document path
    -> Query     -- ^ Query
    -> m (Revision, a)
couchGet db p = couchGetWith AG.fromJSON (mkPath [db, p])  

-- | Put an object in Couch DB with revision, returning the new Revision.
couchPut :: (MonadCouch m, Data a) => 
       Path         -- ^ Database
    -> Path         -- ^ Document path
    -> Revision     -- ^ Document revision. For new docs provide empty string.
    -> Query        -- ^ Query arguments.
    -> a            -- ^ The object to store.
    -> m Revision      
couchPut db p = couchPutWith AG.encode (mkPath [db, p])
    
-- | \"Don't care\" version of 'couchPut'. Creates document only in its 
--   absence.
couchPut_ :: (MonadCouch m, Data a) => 
       Path         -- ^ Database
    -> Path         -- ^ Document path
    -> Query    -- ^ Query arguments.
    -> a           -- ^ The object to store.
    -> m Revision      
couchPut_ db p = couchPutWith_ AG.encode (mkPath [db, p])

-- | Brute force version of 'couchPut'. Creates a document regardless of 
--   presence. 
couchPut' :: (MonadCouch m, Data a) => 
       Path         -- ^ Database
    -> Path         -- ^ Document path
    -> Query    -- ^ Query arguments.
    -> a           -- ^ The object to store.
    -> m Revision      
couchPut' db p = couchPutWith' AG.encode (mkPath [db, p])

------------------------------------------------------------------------------
-- View conduit
------------------------------------------------------------------------------

-- | Convert CouchDB view row or row value from 'Database.CouchDB.Conduit.View' 
--   to concrete type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toType :: (MonadResource m, Data a) => Conduit A.Value m a
toType = toTypeWith AG.fromJSON 