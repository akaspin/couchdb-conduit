{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.CouchDB.Conduit where

-- control
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Exception (Exception)
import Control.Monad.Trans.Class (lift)

-- conduit
import Data.Conduit (ResourceIO, ResourceT)

-- networking
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

-- data
import Data.Typeable (Typeable)
import qualified Data.ByteString as B
import qualified Data.Text as T

-- | A path to a CouchDB Object.
-- 
--   /Note:/ In CouchDB database or path /can/ contain slashes. But, to work 
--   with such objects, path must be escaped.
type Path = String

-- | Represents a revision of a CouchDB Document. 
type Revision = T.Text

-- | Represents a single connection to CouchDB server. 
--
--   To access different databases through a single connection, set 'dbname'
--   to empty string.
data CouchConnection = CouchConnection {
      host      :: B.ByteString     -- ^ Hostname
    , port      :: Int              -- ^ Port
    , manager   :: H.Manager        -- ^ Manager
    , dbname    :: B.ByteString     -- ^ Database name
}

-- | A monad which allows access to the connection
class ResourceIO m => MonadCouch m where
    couchConnection :: m CouchConnection

instance ResourceIO m => MonadCouch (ReaderT CouchConnection m) where
    couchConnection = ask
    
-- | A Couch DB Error. If the error comes from http, the http status code 
--   is also given. Non-http errors include things like errors  
--   parsing the response.
data CouchError = CouchError (Maybe Int) String
    deriving (Show, Typeable)
instance Exception CouchError

-- | The most general method of accessing CouchDB.  This is a very thin wrapper 
--   around 'H.http'.  Most of the time you should use one of the other access 
--   functions, but this function is needed for example to write and read 
--   attachments that are not in JSON format.
-- 
--   Response passed to 'H.ResponseConsumer' unchanged. To protect response 
--   from exceptions use 'protect'.
couch :: (MonadCouch m) =>
           HT.Method                -- ^ Method
        -> B.ByteString             -- ^ Path
        -> HT.RequestHeaders        -- ^ Headers
        -> HT.Query                 -- ^ Query args
        -> H.ResponseConsumer m b   -- ^ Response consumer
        -> H.RequestBody m
        -> ResourceT m b
couch meth path hdrs qs cons reqBody = do
    conn <- lift couchConnection
    let req = H.def 
            { H.method          = meth
            , H.host            = host conn
            , H.requestHeaders  = hdrs
            , H.port            = port conn
            , H.path            = B.intercalate "/" . filter (/="") $ 
                                        [dbname conn, path]
            , H.queryString     = HT.renderQuery False qs
            , H.requestBody     = reqBody }
    H.http req cons (manager conn)

-- | TODO. Protect response from typical errors like 404, 406 e.t.c.
--   
protect :: ResourceIO m => 
       H.ResponseConsumer m b
    -> H.ResponseConsumer m b
protect cons = undefined


runCouch :: ResourceIO m =>
       B.ByteString
    -> Int
    -> B.ByteString
    -> ReaderT CouchConnection m a
    -> m a
runCouch h p d = withCouchConnection h p d . runReaderT

-- | Connect to a CouchDB server, call the supplied function, and then close 
--   the connection.
-- 
--   If you create your own instance of 'MonadCouch' instead of using 
--   'runCouch', this function will help you create the 'CouchConnection'. On 
--   the other hand, if you want to implement connection pooling, you will not 
--   be able to use withCouchConnection and must create the connection yourself.
withCouchConnection :: ResourceIO m =>
       B.ByteString         -- ^ Host
    -> Int                  -- ^ Port
    -> B.ByteString         -- ^ 
    -> (CouchConnection -> m a)
    -> m a
withCouchConnection h p db f = 
     H.withManager $ \m -> lift $ f $ CouchConnection h p m db