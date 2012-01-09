{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{- | 
To work with concrete objects, use the following modules:
  
  * "Database.CouchDB.Conduit.DB"
  
  * "Database.CouchDB.Conduit.View"
  
  * "Database.CouchDB.Conduit.Explicit"
  
  * "Database.CouchDB.Conduit.Generic"
  
  * "Database.CouchDB.Conduit.LowLevel"
   
For complete documentation about The Couch DB HTTP API see
<http://wiki.apache.org/couchdb/Complete_HTTP_API_Reference>
-}



module Database.CouchDB.Conduit (
    -- * Document paths and revisions #path#
    -- $path
    Path,
    mkPath,
    Revision,
    
    -- * CouchDB Connection #connection#
    CouchConnection,
    def,
    couchHost,
    couchPort,
    couchManager,
    couchDB,
    
    -- * Runtime enviroment and errors #runtime#
    -- $runtime
    MonadCouch (..),
    CouchError (..),
    runCouch,
    withCouchConnection,
    
) where

import              Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import              Control.Exception (Exception)
import              Control.Monad.Trans.Class (lift)

import              Data.Conduit (ResourceIO, ResourceT, runResourceT)

import qualified    Network.HTTP.Conduit as H
import qualified    Network.HTTP.Types as HT

import              Data.Generics (Typeable)
import              Data.Default (Default (def))
import qualified    Data.ByteString as B
import qualified    Data.Text.Encoding as TE
import qualified    Blaze.ByteString.Builder as BLB

-----------------------------------------------------------------------------
-- Paths
-----------------------------------------------------------------------------

-- $path 
-- As a rule, full path to document in CouchDB is just URL path. But there is 
-- one subtlety. For example, document ids /can/ contain slashes. But, 
-- to work with such objects, path fragments must be escaped.
-- 
-- > database/doc%2Fname
--
-- But, fo non-document items such as views, attachments e.t.c., slashes
-- between path fragments /must not/ be escaped. While slashes in path 
-- fragments /must/ be escaped.
-- 
-- > database/_design/my%2Fdesign/_view/my%2Fview

-- | Represents a path or path fragment.
type Path = B.ByteString

-- | Represents a revision of a CouchDB Document. 
type Revision = B.ByteString

-- | Make correct path from escaped fragments. Filter empty fragments.
--
-- > mkPath ["db", "", "doc/with/slashes"]
-- > db/doc%2Fwith%2Fslashes
mkPath :: [Path]    -- ^ Path fragments be escaped.  
       -> Path
mkPath = BLB.toByteString . HT.encodePathSegments . 
    map TE.decodeUtf8 . filter (/="")

-----------------------------------------------------------------------------
-- Connection
-----------------------------------------------------------------------------

-- | Represents a single connection to CouchDB server. The constructor for this 
--   data type is not exposed. Instead, you should use either the 'def' method 
--   to retrieve a default instance.
data CouchConnection = CouchConnection {
      couchHost :: B.ByteString     
        -- ^ Hostname. Default value is \"localhost\"
    , couchPort :: Int              
        -- ^ Port. 5984 by default.
    , couchManager :: Maybe H.Manager  
        -- ^ Connection 'Manager'. 'Nothing' by default. If you need to use
        --   your 'H.Manager' (for connection pooling for example), set it to
        --   'Just' 'H.Manager'.
    , couchDB :: Path             
        -- ^ Database name. This value is prepended to 'Path' to form the full 
        --   path in all requests.
        --    
        --   By default is 'B.empty'. This makes it possible to access 
        --   different databases through a single connection. But, in this 
        --   case, all requests must be preceded by the database name with 
        --   unescaped slash. See 'Path' for details.
}

instance Default CouchConnection where
    def = CouchConnection "localhost" 5984 Nothing B.empty

-----------------------------------------------------------------------------
-- Runtime
-----------------------------------------------------------------------------

-- $runtime
-- All functions to access CouchDB require a 'MonadCouch' instance to 
-- access the connection information.  'ReaderT' is an instance of 
-- 'MonadCouch', and /runCouch/ runs a sequence of database actions using 
-- 'ReaderT'.
-- 
-- If your db code is part of a larger monad, it makes sense to just make the 
-- larger monad an instance of 'MonadCouch' and skip the intermediate ReaderT, 
-- since then performance is improved by eliminating one monad from the final 
-- transformer stack.

-- | A monad which allows access to the connection.
class ResourceIO m => MonadCouch m where
    couchConnection :: m CouchConnection

instance (ResourceIO m) => MonadCouch (ReaderT CouchConnection m) where
    couchConnection = ask

-- | A Couch DB Error. If the error comes from http, the http status code 
--   is also given. Non-http errors include things like errors  
--   parsing the response.
data CouchError = CouchError (Maybe Int) String
    deriving (Show, Typeable)
instance Exception CouchError

-- | Run a sequence of CouchDB actions. This function is a combination of 
--   'withCouchConnection' and 'runReaderT'.
--  
--   If you create your own instance of 'MonadCouch', use 'withCouchConnection'.  
runCouch :: ResourceIO m =>
       CouchConnection                            -- ^ Couch connection
    -> ResourceT (ReaderT CouchConnection m) a    -- ^ CouchDB actions
    -> m a
runCouch c = withCouchConnection c . runReaderT . runResourceT

-- | Connect to a CouchDB server, call the supplied function, and then close 
--   the connection.
-- 
-- > withCouchConnection def {couchDB = "db"} . runReaderT . runResourceT $ do
-- >    ... -- actions
withCouchConnection :: ResourceIO m =>
       CouchConnection              -- ^ Couch connection
    -> (CouchConnection -> m a)     -- ^ Function to run
    -> m a
withCouchConnection c@(CouchConnection _ _ mayMan _) f = 
    case mayMan of
        Nothing -> H.withManager $ \m -> lift $ f $ c {couchManager = Just m}
        Just m -> runResourceT $ lift $ f $ c {couchManager = Just m}

