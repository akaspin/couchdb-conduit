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
--   For details of types see "Data.Aeson". To work with documents in 
--   generic manner, look at "Database.CouchDB.Conduit.Generic".

module Database.CouchDB.Conduit.Explicit (
    -- * Accessing documents
    couchGet,
    -- * Manipulating documents
    couchPut,
    couchPut_,
    couchPut',
    -- * Working with views #view#
    toType
) where

import qualified Data.Aeson as A
import Data.Conduit (Conduit(..), MonadResource)

import Network.HTTP.Types (Query)

import Database.CouchDB.Conduit.Internal.Connection (MonadCouch(..), 
        Path, Revision, mkPath)
import Database.CouchDB.Conduit.Internal.Doc (couchGetWith, couchPutWith, 
        couchPutWith_, couchPutWith') 
import Database.CouchDB.Conduit.Internal.View (toTypeWith)

------------------------------------------------------------------------------
-- Document
------------------------------------------------------------------------------

-- | Load a single 'A.ToJSON' object with 'Revision' from couch DB. 
couchGet :: (MonadCouch m, A.FromJSON a) => 
       Path         -- ^ Database
    -> Path         -- ^ Document path
    -> Query     -- ^ Query
    -> m (Revision, a)
couchGet db p = couchGetWith A.fromJSON (mkPath [db, p])

-- | Put an 'A.FromJSON' object in Couch DB with revision, returning the 
--   new 'Revision'.
couchPut :: (MonadCouch m, A.ToJSON a) => 
       Path         -- ^ Database
    -> Path         -- ^ Document path
    -> Revision    -- ^ Document revision. For new docs provide empty string.
    -> Query    -- ^ Query arguments.
    -> a           -- ^ The object to store.
    -> m Revision      
couchPut db p = couchPutWith A.encode (mkPath [db, p])

-- | \"Don't care\" version of 'couchPut'. Creates document only in its 
--   absence.
couchPut_ :: (MonadCouch m, A.ToJSON a) => 
       Path         -- ^ Database
    -> Path         -- ^ Document path
    -> Query    -- ^ Query arguments.
    -> a           -- ^ The object to store.
    -> m Revision      
couchPut_ db p = couchPutWith_ A.encode (mkPath [db, p])

-- | Brute force version of 'couchPut'. Creates a document regardless of 
--   presence. 
couchPut' :: (MonadCouch m, A.ToJSON a) => 
       Path         -- ^ Database
    -> Path         -- ^ Document path
    -> Query    -- ^ Query arguments.
    -> a           -- ^ The object to store.
    -> m Revision      
couchPut' db p = couchPutWith' A.encode (mkPath [db, p])

------------------------------------------------------------------------------
-- View conduit
------------------------------------------------------------------------------

-- | Convert CouchDB view row or row value from "Database.CouchDB.Conduit.View" 
--   to concrete 'A.FromJSON' type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toType :: (MonadResource m, A.FromJSON a) => Conduit A.Value m a
toType = toTypeWith A.fromJSON 



