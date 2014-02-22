{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

-- | CouchDB connection. 

module Database.CouchDB.Conduit.Internal.Connection (
    -- * Document paths and revisions #path#
    Path,
    mkPath,
    Revision,
    
    -- * CouchDB Connection #connection#
    CouchConnection,
    def,
    couchHost,
    couchPort,
    couchLogin,
    couchPass,
    couchPrefix,
    
    -- * Runtime enviroment and errors #runtime#
    MonadCouch (..),
    CouchError (..),
    runCouch,
    withCouchConnection
    
) where

import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Exception (Exception)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadBaseControl, MonadResourceBase, 
        MonadResource, ResourceT, runResourceT)
 
import Data.Generics (Typeable)
import Data.Default (Default (def))
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import qualified Blaze.ByteString.Builder as BLB


import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

-----------------------------------------------------------------------------
-- Paths
-----------------------------------------------------------------------------

-- | Represents a path or path fragment.
--
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
--
-- Except low-level functions, @couchdb-conduit@ escapes all segments in paths.  
type Path = B.ByteString

-- | Represents a revision of a CouchDB Document. 
type Revision = B.ByteString

-- | Make correct path and escape fragments. Filter empty fragments.
--
-- > mkPath ["db", "", "doc/with/slashes"]
-- > /db/doc%2Fwith%2Fslashes
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
    , couchLogin :: B.ByteString
        -- ^ CouchDB login. By default is 'B.empty'.
    , couchPass :: B.ByteString
        -- ^ CouchDB password. By default is 'B.empty'.
    , couchPrefix :: B.ByteString
        -- ^ CouchDB database prefix. It will prepended to first fragment of
        --   request path. Must be fully valid DB name fragment.
}

instance Default CouchConnection where
    def = CouchConnection "localhost" 5984 B.empty B.empty B.empty

-----------------------------------------------------------------------------
-- Runtime
-----------------------------------------------------------------------------

-- | A monad which allows access to the connection.
-- 
-- All functions to access CouchDB require a 'MonadCouch' instance to 
-- access the connection information.  'ReaderT' is an instance of 
-- 'MonadCouch', and /runCouch/ runs a sequence of database actions using 
-- 'ReaderT' and 'ResourceT'.
-- 
-- If your db code is part of a larger monad, it makes sense to just make the 
-- larger monad an instance of 'MonadCouch' and skip the intermediate ReaderT, 
-- since then performance is improved by eliminating one monad from the final 
-- transformer stack.
class (MonadResource m, MonadBaseControl IO m) => MonadCouch m where
    couchConnection :: m (H.Manager, CouchConnection)

instance (MonadResource m, MonadBaseControl IO m) => 
        MonadCouch (ReaderT (H.Manager, CouchConnection) m) where
    couchConnection = ask

--No instance for (MonadCouch (ResourceT (ReaderT 
-- (H.Manager, CouchConnection) (ResourceT IO)))) 

-- | A CouchDB Error.
data CouchError 
    = CouchHttpError Int B.ByteString
        -- ^ Error comes from http.
    | CouchInternalError B.ByteString
        -- ^ Non-http errors include things like errors  
        --   parsing the response.
    | NotModified
        -- ^ /Is not an error actually/. It is thrown when CouchDB returns 
        --   @304 - Not Modified@ response to the request. See 
        --   <http://wiki.apache.org/couchdb/HTTP_Document_API>
  deriving (Show, Typeable)
instance Exception CouchError

-- | Connect to a CouchDB server, run a sequence of CouchDB actions, and then 
--   close the connection.. This function is a combination of 'H.withManager', 
--   'withCouchConnection', 'runReaderT' and 'runResourceT'.
--  
--   If you create your own instance of 'MonadCouch' or use connection pool, 
--   use 'withCouchConnection'.  
runCouch :: (MonadResourceBase m) =>
       CouchConnection            -- ^ Couch connection
    -> ReaderT (H.Manager, CouchConnection) (ResourceT m) a 
                                  -- ^ Actions
    -> m a
runCouch c f = H.withManager $ \manager -> 
    withCouchConnection manager c . runReaderT . runResourceT . lift $ f

-- | Run a sequence of CouchDB actions with provided 'H.Manager' and 
--   'CouchConnection'. 
-- 
-- > withCouchConnection manager def {couchDB = "db"} . runReaderT . 
-- >          runResourceT . lift $ do
-- >    ... -- actions
withCouchConnection :: (MonadResource m, MonadBaseControl IO m) => 
       H.Manager            -- ^ Connection manager
    -> CouchConnection      -- ^ Couch connection
    -> ((H.Manager, CouchConnection) -> m a) 
                            -- ^ Actions
    -> m a
withCouchConnection man c@(CouchConnection{}) f = 
    f (man, c)

