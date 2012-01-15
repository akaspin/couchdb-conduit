{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | Explicit methods for CouchDB documents. Documents represents in \"good
--   old\" aeson manner through 'A.ToJSON' and 'A.FromJSON'.
--
-- > {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- >
-- > import Control.Applicative ((<$>), (<*>))
-- > import Control.Monad.IO.Class (liftIO)
-- > import Data.Aeson
-- > import Database.CouchDB.Conduit
-- > import Database.CouchDB.Conduit.Explicit
-- >
-- > -- | Our doc with instances
-- > data D = D { f1 :: Int, f2 :: String } deriving (Show)
-- > 
-- > instance FromJSON D where
-- >    parseJSON (Object v) = D <$> v .: "f1" <*> v .: "f2"
-- >    parseJSON _          = mzero
-- >
-- > instance ToJSON D where
-- >    toJSON (D f1 f2) = object ["f1" .= f1, "f2" .= f2]
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
--   For details of types see "Data.Aeson". To work with documents in 
--   generic manner, look at "Database.CouchDB.Conduit.Generic".

module Database.CouchDB.Conduit.Explicit (
    -- * Accessing documents
    couchGet,
    couchRev,
    couchRev',
    -- * Manipulating documents
    couchPut,
    couchPut_,
    couchPut',
    couchDelete,
    -- * Working with views #view#
    toType
) where

import qualified    Data.Aeson as A
import              Data.Conduit (ResourceT, Conduit(..), ResourceIO)

import              Network.HTTP.Types as HT

import              Database.CouchDB.Conduit (MonadCouch(..), Path, Revision)
import              Database.CouchDB.Conduit.Internal.Doc 
import              Database.CouchDB.Conduit.Internal.View (toTypeWith)

------------------------------------------------------------------------------
-- Document
------------------------------------------------------------------------------

-- | Load a single 'A.ToJSON' object with 'Revision' from couch DB. 
couchGet :: (MonadCouch m, A.FromJSON a) => 
       Path         -- ^ Document path
    -> HT.Query     -- ^ Query
    -> ResourceT m (Revision, a)
couchGet = couchGetWith A.fromJSON

-- | Put an 'A.FromJSON' object in Couch DB with revision, returning the 
--   new 'Revision'.
couchPut :: (MonadCouch m, A.ToJSON a) => 
        Path        -- ^ Document path.
     -> Revision    -- ^ Document revision. For new docs provide empty string.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> ResourceT m Revision      
couchPut = couchPutWith A.encode

-- | \"Don't care\" version of 'couchPut'. Creates document only in its 
--   absence.
couchPut_ :: (MonadCouch m, A.ToJSON a) => 
        Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> ResourceT m Revision      
couchPut_ = couchPutWith_ A.encode

-- | Brute force version of 'couchPut'. Creates a document regardless of 
--   presence. 
couchPut' :: (MonadCouch m, A.ToJSON a) => 
        Path        -- ^ Document path.
     -> HT.Query    -- ^ Query arguments.
     -> a           -- ^ The object to store.
     -> ResourceT m Revision      
couchPut' = couchPutWith' A.encode

------------------------------------------------------------------------------
-- View conduit
------------------------------------------------------------------------------

-- | Convert CouchDB view row or row value from "Database.CouchDB.Conduit.View" 
--   to concrete 'A.FromJSON' type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toType :: (ResourceIO m, A.FromJSON a) => Conduit A.Value m a
toType = toTypeWith A.fromJSON 



