{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.CouchDB.Conduit (
    Path,
    Revision,
    CouchConnection(..),
    MonadCouch(..),
    CouchError(..),
    couch,
    protect,
    runCouch,
    withCouchConnection
) where

import Prelude hiding (catch)

-- control
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Exception (Exception, SomeException)
import Control.Exception.Lifted (catch, throwIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Base (liftBase)

-- conduit
import Data.Conduit (ResourceIO, ResourceT, ($$))
import Data.Conduit.Attoparsec (sinkParser)

-- networking
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

-- data
import Data.Typeable (Typeable)
import Data.Aeson (json, Value(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU8 (toString)
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M (lookup)

-- | A path to a CouchDB Object.
-- 
--   /Note:/ In CouchDB database or path /can/ contain slashes. But, to work 
--   with such objects, path must be escaped.
type Path = B.ByteString

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
    , dbname    :: Path             -- ^ Database name
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
        -> Path                     -- ^ Path
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

-- | Protect response from typical errors like 404, 406 e.t.c. Only responses 
--   with codes 200, 201, 202 and 304 are passed. 
protect :: ResourceIO m => 
       H.ResponseConsumer m b
    -> H.ResponseConsumer m b
protect c st@(HT.Status 200 _) hdrs bsrc = c st hdrs bsrc
protect c st@(HT.Status 201 _) hdrs bsrc = c st hdrs bsrc
protect c st@(HT.Status 202 _) hdrs bsrc = c st hdrs bsrc
protect c st@(HT.Status 304 _) hdrs bsrc = c st hdrs bsrc
protect _ (HT.Status sCode sMsg) _ bsrc = do
    v <- catch (bsrc $$ sinkParser json) 
               (\(_::SomeException) -> return Null)
    liftBase $ throwIO $ CouchError (Just sCode) $ msg v
  where 
    msg v = BU8.toString sMsg ++ reason v
    reason (Object v) = case M.lookup "reason" v of
            Just (String t) -> ": " ++ T.unpack t
            _                 -> ""
    reason _ = []

-- | Run a sequence of CouchDB actions.
--
--   The functions below to access CouchDB require a 'MonadCouch' instance to 
--   access the connection information.  'ReaderT' is an instance of 
--   'MonadCouch', and /runCouch/ runs a sequence of database actions using 
--   'ReaderT'.  See the top of this page for an example using /runCouch/.
--
--   The main reason to not use /runCouch/ is to obtain more control over 
--   connection pooling. Also, if your db code is part of a larger monad, it 
--   makes sense to just make the larger monad an instance of 'MonadCouch' and 
--   skip the intermediate ReaderT, since then performance is improved by 
--   eliminating one monad from the final transformer stack.
--
--   This function is a combination of 'withCouchConnection' and 'runReaderT'
runCouch :: ResourceIO m =>
       B.ByteString                 -- ^ Host
    -> Int                          -- ^ Port
    -> Path                         -- ^ Database
    -> ReaderT CouchConnection m a  -- ^ CouchDB actions
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
       B.ByteString                 -- ^ Host
    -> Int                          -- ^ Port
    -> Path                         -- ^ Database 
    -> (CouchConnection -> m a)     -- ^ Function to run
    -> m a
withCouchConnection h p db f = 
     H.withManager $ \m -> lift $ f $ CouchConnection h p m db