{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses#-}

-- | Low-lewel API.

module Database.CouchDB.Conduit.Connection  where

import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp)
import Control.Monad.Trans.Resource

import qualified Data.Conduit
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Data.Text as T
import Control.Monad.Trans.Class (lift)

-- | A path to a Couch DB Object.
type Path = String

-- | Represents a revision of a Couch DB Document.
type Revision = T.Text

-- | Represents a single connection to CouchDB server. 
data CouchConnection = CouchConnection {
      host      :: B.ByteString     -- ^ Hostname
    , port      :: Int              -- ^ Port
    , manager   :: H.Manager        -- ^ Manager
    , dbname    :: String           -- ^ Database name
}

-- | A monad which allows access to the connection.
class (ResourceIO m) => MonadCouch m where
    couchConnection :: m CouchConnection

instance (ResourceIO m) => 
    MonadCouch (ReaderT CouchConnection m) where
    couchConnection = ask
    
--withCouchConnection :: (ResourceIO m) => 
--        String                    -- ^ Host
--     -> Int                       -- ^ Port
--     -> String                    -- ^ Database name. Just set empty if you 
--                                  --   need access to many DBs
--     -> (CouchConnection -> ResourceT m a)  -- ^ Function to run
--     -> m a
withCouchConnection h p db f = 
     H.withManager $ \m -> f $ CouchConnection (BU8.fromString h) p m db
     
--runCouch :: (ResourceIO m) => 
--        String                      -- ^ Host
--     -> Int                         -- ^ Port
--     -> String                      -- ^ Database name. Just set empty if you 
--                                    --   need access to many DBs
--     -> ReaderT CouchConnection (ResourceT m) a -- ^ CouchDB actions
--     -> m a
runCouch h p d = withCouchConnection h p d . runReaderT

couch meth path hdrs qs acts reqBody = do
    conn <- couchConnection
    let req = H.def 
            { H.method      = meth
            , H.host        = host conn
            , H.requestHeaders = hdrs
            , H.port        = port conn
            , H.path        = BU8.fromString ("/" ++ dbname conn ++ "/" ++ path)
            , H.queryString = qs
            , H.requestBody = reqBody }
    lift $ H.http req (handleResponse acts) (manager conn)
    
handleResponse acts (HT.Status 200 _) hdrs = acts hdrs
--handleResponse acts sc hdrs bsrc = undefined


