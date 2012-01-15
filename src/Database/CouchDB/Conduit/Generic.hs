{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | Generic methods for CouchDB documents. Unlike explicit, generic methods 
--   uses "Data.Generic".
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
-- > runCouch def {couchDB="mydb"} $ do
-- >    -- Put new doc and update it
-- >    rev1 <- couchPut "my-doc1" "" [] $ D 123 "str"         
-- >    rev2 <- couchPut "my-doc1" rev1 [] $ D 1234 "another"
-- >
-- >    -- get it and print
-- >    (rev3, d1 :: D) <- couchGet "my-doc1" [] 
-- >    liftIO $ print d1
-- >
-- >    -- update it in brute-force manner    
-- >    couchPut' "my-doc1" [] $ D 12345 "third"    -- notice - no rev
-- >    
-- >    -- get revision and delete
-- >    rev3 <- couchRev "my-doc1"
-- >    couchDelete "my-doc1" rev3
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
    couchRev,
    -- * Manipulating documents
    couchPut,
    couchPut_,
    couchPut',
    couchDelete,
    -- * Working with views #view#
    toType
) where

import              Data.Generics (Data)
import qualified    Data.Aeson as A
import qualified    Data.Aeson.Generic as AG
import              Data.Conduit (ResourceT, Conduit(..), ResourceIO)

import qualified    Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import              Database.CouchDB.Conduit.Internal.Doc
import              Database.CouchDB.Conduit.Internal.View

-- | Load a single object from couch DB.
couchGet :: (MonadCouch m, Data a) => 
       Path         -- ^ Document path
    -> HT.Query     -- ^ Query
    -> ResourceT m (Revision, a)
couchGet = couchGetWith AG.fromJSON  

-- | Put an object in Couch DB with revision, returning the new Revision.
couchPut :: (MonadCouch m, Data a) => 
        Path        -- ^ Document path.
     -> Revision    -- ^ Document revision. For new docs provide empty string.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> ResourceT m Revision      
couchPut = couchPutWith AG.encode
    
-- | \"Don't care\" version of 'couchPut'. Creates document only in its 
--   absence.
couchPut_ :: (MonadCouch m, Data a) => 
        Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> ResourceT m Revision      
couchPut_ = couchPutWith_ AG.encode

-- | Brute force version of 'couchPut'. Creates a document regardless of 
--   presence. 
couchPut' :: (MonadCouch m, Data a) => 
        Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> ResourceT m Revision      
couchPut' = couchPutWith' AG.encode

------------------------------------------------------------------------------
-- View conduit
------------------------------------------------------------------------------

-- | Convert CouchDB view row or row value from 'Database.CouchDB.Conduit.View' 
--   to concrete type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toType :: (ResourceIO m, Data a) => Conduit A.Value m a
toType = toTypeWith AG.fromJSON 