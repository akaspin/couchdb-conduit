{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 
--   
--   For complete documentation about The Couch DB HTTP API see
--   <http://wiki.apache.org/couchdb/Complete_HTTP_API_Reference>

module Database.CouchDB.Conduit (
    -- * Document paths and revisions
    -- $docPath
    Path,
    mkPath,
    Revision,
    
    -- * CouchDB Connection
    CouchConnection,
    def,
    couchHost,
    couchPort,
    couchManager,
    couchDB,
    
    -- * Running enviroment and errors
    MonadCouch (..),
    CouchError (..),
    runCouch,
    withCouchConnection,
    
) where

import              Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import              Control.Exception (Exception)
import              Control.Monad.Trans.Class (lift)

import              Data.Conduit (ResourceIO, runResourceT)

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
-- Paths
-----------------------------------------------------------------------------

-- $run
-- 

-- | A monad which allows access to the connection.
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
--   'runCouch', this function will help you create the 'CouchConnection'. 
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
     
---- | CouchDB response
--type CouchResponse m = H.Response (BufferedSource m B.ByteString)
--
---- | The most general method of accessing CouchDB.  This is a very thin wrapper 
----   around 'H.http'.  Most of the time you should use one of the other access 
----   functions, but this function is needed for example to write and read 
----   attachments that are not in JSON format.
--couch :: MonadCouch m =>
--       HT.Method                -- ^ Method
--    -> Path                     -- ^ Path
--    -> HT.RequestHeaders        -- ^ Headers
--    -> HT.Query                 -- ^ Query args
--    -> H.RequestBody m          -- ^ Request body
--    -> (CouchResponse m -> ResourceT m (CouchResponse m))
--                                -- ^ Protect function. See 'protect'
--    -> ResourceT m (CouchResponse m)
--couch meth path hdrs qs reqBody protectFn = do
--    conn <- lift couchConnection
--    let req = H.def 
--            { H.method          = meth
--            , H.host            = couchHost conn
--            , H.requestHeaders  = hdrs
--            , H.port            = couchPort conn
--            , H.path            = B.intercalate "/" . filter (/="") $ 
--                                        [couchDB conn, path]
--            , H.queryString     = HT.renderQuery False qs
--            , H.requestBody     = reqBody
--            , H.checkStatus = const . const $ Nothing }
--    -- FIXME fromMaybe
--    res <- H.http req (fromJust $ couchManager conn)
--    protectFn res 
--
---- | Protect 'H.Response' from bad status codes. If status code in list 
----   of status codes - just return response. Otherwise - throw 'CouchError'.
----   
----   Instead 'H.checkStatus', 'protect' parses CouchDB response body JSON and
----   extract \"reason\" message.
----   
----   To protect from typical errors use 'protect''.
--protect :: MonadCouch m => 
--       [Int]                                        -- ^ Good codes
--    -> CouchResponse m   -- ^ Response
--    -> ResourceT m (CouchResponse m)
--protect goodCodes ~resp@(H.Response (HT.Status sc sm) _ bsrc)  
--    | sc `elem` goodCodes = return resp
--    | otherwise = do
--        v <- catch (bsrc $$ sinkParser json)
--                   (\(_::SomeException) -> return Null)
--        liftBase $ resourceThrow $ CouchError (Just sc) $ msg v
--        where 
--        msg v = BU8.toString sm ++ reason v
--        reason (Object v) = case M.lookup "reason" v of
--                Just (String t) -> ": " ++ T.unpack t
--                _                 -> ""
--        reason _ = []
--
---- | Protect from typical status codes: 200, 201, 202 and 304. See 'protect'
----   fo details.       
--protect' :: MonadCouch m => 
--       CouchResponse m   -- ^ Response
--    -> ResourceT m (CouchResponse m)
--protect' = protect [200, 201, 202, 304]
--
--
--
--
