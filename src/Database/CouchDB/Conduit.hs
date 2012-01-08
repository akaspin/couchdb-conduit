{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 
--   
--   For complete documentation about The Couch DB HTTP API see
--   <http://wiki.apache.org/couchdb/Complete_HTTP_API_Reference>

module Database.CouchDB.Conduit (
    -- * Document paths and revisions
    -- $docPath
    Revision,
    Path,
    mkPath,
    
    -- * CouchDB Connection
    CouchConnection,
    def,
    couchHost,
    couchPort,
    couchManager,
    couchDB,
    
    -- * Executing
    runCouch,
    withCouchConnection,
    MonadCouch (..),
    CouchError (..),
    
    -- * Low-level API
    couch,
    protect,
    protect'
) where

import Prelude hiding (catch)

import              Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import              Control.Exception (Exception, SomeException)
import              Control.Exception.Lifted (catch)
import              Control.Monad.Trans.Class (lift)
import              Control.Monad.Base (liftBase)

import              Data.Conduit (ResourceIO, ResourceT, BufferedSource, 
                        runResourceT, ($$), resourceThrow)
import              Data.Conduit.Attoparsec (sinkParser)

import qualified    Network.HTTP.Conduit as H
import qualified    Network.HTTP.Types as HT

import              Data.Generics (Typeable)
import              Data.Default (Default (def))
import              Data.Aeson (json, Value(..))
import qualified    Data.ByteString as B
import qualified    Data.ByteString.UTF8 as BU8
import qualified    Data.Text as T (unpack)
import qualified    Data.Text.Encoding as TE
import qualified    Blaze.ByteString.Builder as BLB
import qualified    Data.HashMap.Lazy as M
import              Data.Maybe (fromJust)

-- | Path or path fragment.
type Path = B.ByteString

-- | Represents a revision of a CouchDB Document. 
type Revision = B.ByteString

-- | Represents a single connection to CouchDB server. 
--
--   To access different databases through a single connection, set 'dbname'
--   to empty string. But, in this case, all requests must be preceded by the 
--   database name with unescaped slash.
data CouchConnection = CouchConnection {
      couchHost      :: B.ByteString     -- ^ Hostname
    , couchPort      :: Int              -- ^ Port
    , couchManager   :: Maybe H.Manager  -- ^ Manager
    , couchDB        :: Path             -- ^ Database name
}

instance Default CouchConnection where
    def = CouchConnection "localhost" 5984 Nothing ""

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

-- | Make correct path from escaped fragments. Filter empty fragments.
--
-- > mkPath ["db", "", "doc/with/slashes"]
-- > db/doc%2Fwith%2Fslashes
mkPath :: [Path]    -- ^ Path fragments be escaped.  
       -> Path
mkPath = BLB.toByteString . HT.encodePathSegments . 
    map TE.decodeUtf8 . filter (/="")

-- | The most general method of accessing CouchDB.  This is a very thin wrapper 
--   around 'H.http'.  Most of the time you should use one of the other access 
--   functions, but this function is needed for example to write and read 
--   attachments that are not in JSON format.
couch :: MonadCouch m =>
       HT.Method                -- ^ Method
    -> Path                     -- ^ Path
    -> HT.RequestHeaders        -- ^ Headers
    -> HT.Query                 -- ^ Query args
    -> H.RequestBody m          -- ^ Request body
    -> (H.Response (BufferedSource m B.ByteString)   
        -> ResourceT m (H.Response (BufferedSource m B.ByteString)))
                                -- ^ Protect function. See 'protect'
    -> ResourceT m (H.Response (BufferedSource m B.ByteString))
couch meth path hdrs qs reqBody protectFn = do
    conn <- lift couchConnection
    let req = H.def 
            { H.method          = meth
            , H.host            = couchHost conn
            , H.requestHeaders  = hdrs
            , H.port            = couchPort conn
            , H.path            = B.intercalate "/" . filter (/="") $ 
                                        [couchDB conn, path]
            , H.queryString     = HT.renderQuery False qs
            , H.requestBody     = reqBody
            , H.checkStatus = const . const $ Nothing }
    -- FIXME fromMaybe
    res <- H.http req (fromJust $ couchManager conn)
    protectFn res 

-- | Protect 'H.Response' from bad status codes. If status code in list 
--   of status codes - just return response. Otherwise - throw 'CouchError'.
--   
--   Instead 'H.checkStatus', 'protect' parses CouchDB response body JSON and
--   extract \"reason\" message.
--   
--   To protect from typical errors use 'protect''.
protect :: MonadCouch m => 
       [Int]                                        -- ^ Good codes
    -> H.Response (BufferedSource m B.ByteString)   -- ^ Response
    -> ResourceT m (H.Response (BufferedSource m B.ByteString))
protect goodCodes ~resp@(H.Response (HT.Status sc sm) _ bsrc)  
    | sc `elem` goodCodes = return resp
    | otherwise = do
        v <- catch (bsrc $$ sinkParser json)
                   (\(_::SomeException) -> return Null)
        liftBase $ resourceThrow $ CouchError (Just sc) $ msg v
        where 
        msg v = BU8.toString sm ++ reason v
        reason (Object v) = case M.lookup "reason" v of
                Just (String t) -> ": " ++ T.unpack t
                _                 -> ""
        reason _ = []

-- | Protect from typical status codes: 200, 201, 202 and 304. See 'protect'
--   fo details.       
protect' :: MonadCouch m => 
       H.Response (BufferedSource m B.ByteString)   -- ^ Response
    -> ResourceT m (H.Response (BufferedSource m B.ByteString))
protect' = protect [200, 201, 202, 304]

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
       CouchConnection              -- ^ Couch connection
    -> ReaderT CouchConnection m a  -- ^ CouchDB actions
    -> m a
runCouch c = withCouchConnection c . runReaderT

-- | Connect to a CouchDB server, call the supplied function, and then close 
--   the connection.
-- 
--   If you create your own instance of 'MonadCouch' instead of using 
--   'runCouch', this function will help you create the 'CouchConnection'. On 
--   the other hand, if you want to implement connection pooling, you will not 
--   be able to use withCouchConnection and must create the connection yourself.
-- 
-- > withCouchConnection def {couchDB = "db"} $ runReaderT $ do
-- >    ... -- actions
withCouchConnection :: ResourceIO m =>
       CouchConnection              -- ^ Couch connection
    -> (CouchConnection -> m a)     -- ^ Function to run
    -> m a
withCouchConnection c@(CouchConnection _ _ mayMan _) f = 
    case mayMan of
        Nothing -> H.withManager $ \m -> lift $ f $ c {couchManager = Just m}
        Just m -> runResourceT $ lift $ f $ c {couchManager = Just m}
     
-- $docPath
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

