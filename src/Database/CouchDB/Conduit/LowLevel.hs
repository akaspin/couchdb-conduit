{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse #-}

-- | Low-level method and tools of accessing CouchDB.

module Database.CouchDB.Conduit.LowLevel (
    -- * Response
    CouchResponse,
    
    -- * Low-level access
    couch,
    couch',
    
    -- * Response protection
    protect
    ,
    protect'
) where

import              Prelude

import Control.Exception.Lifted (catch, throw)
import Control.Exception (SomeException)

import qualified Data.ByteString as B
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as M
import Data.String.Conversions ((<>), cs)

import Data.Conduit (ResumableSource, ($$+-))
import Data.Conduit.Attoparsec (sinkParser)
                        
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit.Internal.Connection

-- | CouchDB response
type CouchResponse m = H.Response (ResumableSource m B.ByteString)

-- | The most general method of accessing CouchDB.  This is a very thin wrapper 
--   around 'H.http'.  Most of the time you should use one of the other access 
--   functions, but this function is needed for example to write and read 
--   attachments that are not in JSON format.
couch :: MonadCouch m =>
       HT.Method                -- ^ Method
    -> Path                     -- ^ Correct 'Path' with escaped fragments.
                                --   'couchPrefix' will be prepended to path.
    -> HT.RequestHeaders        -- ^ Headers
    -> HT.Query                 -- ^ Query args
    -> H.RequestBody m          -- ^ Request body
    -> (CouchResponse m -> m (CouchResponse m))
                                -- ^ Protect function. See 'protect'
    -> m (CouchResponse m)
couch meth path = 
    couch' meth withPrefix
  where
    withPrefix prx 
        | B.null prx = path
        | otherwise = "/" <> prx <> B.tail path 

-- | More generalized version of 'couch'. Instead 'Path' it takes function
--   what takes prefix and returns a path.
couch' :: MonadCouch m =>
       HT.Method                -- ^ Method
    -> (Path -> Path)           -- ^ 'couchPrefix'->Path function. Output must 
                                --   be correct 'Path' with escaped fragments.
    -> HT.RequestHeaders        -- ^ Headers
    -> HT.Query                 -- ^ Query args
    -> H.RequestBody m          -- ^ Request body
    -> (CouchResponse m -> m (CouchResponse m))
                                -- ^ Protect function. See 'protect'
    -> m (CouchResponse m)
couch' meth pathFn hdrs qs reqBody protectFn =  do
    (manager, conn) <- couchConnection
    let req = H.def 
            { H.method          = meth
            , H.host            = couchHost conn
            , H.requestHeaders  = hdrs
            , H.port            = couchPort conn
            , H.path            = pathFn $ couchPrefix conn
            , H.queryString     = HT.renderQuery False qs
            , H.requestBody     = reqBody
            , H.checkStatus = const . const . const $ Nothing }
    -- Apply auth if needed
    let req' = if couchLogin conn == B.empty then req else H.applyBasicAuth 
            (couchLogin conn) (couchPass conn) req
    res <- H.http req' manager
    protectFn res

-- | Protect 'H.Response' from bad status codes. If status code in list 
--   of status codes - just return response. Otherwise - throw 'CouchError'.
--   
--   Instead 'H.checkStatus', 'protect' parses CouchDB response body JSON and
--   extract \"reason\" message.
--   
--   To protect from typical errors use 'protect''.
protect :: MonadCouch m => 
       [Int]             -- ^ Good codes
    -> (CouchResponse m -> m (CouchResponse m)) -- ^ handler
    -> CouchResponse m   -- ^ Response
    -> m (CouchResponse m)
protect goodCodes h resp
    | (H.responseStatus resp) == HT.status304 = throw NotModified
    | (H.responseStatus resp) `elem` goodCodes = h resp
    | otherwise = do
        v <- catch ((H.responseBody resp) $$+- sinkParser A.json)
                   (\(_::SomeException) -> return A.Null)
        throw $ CouchHttpError (HT.statusCode $ H.responseStatus resp) $ msg v
        where 
        msg v = (HT.statusMessage $ H.responseStatus resp) <> reason v
        reason (A.Object v) = case M.lookup "reason" v of
                Just (A.String t) -> ": " <> cs t
                _                 -> ""
        reason _ = B.empty

-- | Protect from typical status codes. It's equivalent of
--
--   > protect [200, 201, 202, 304] return
--
--   See 'protect' for details.       
protect' :: MonadCouch m => 
       CouchResponse m   -- ^ Response
    -> m (CouchResponse m)
protect' = protect [200, 201, 202, 304] return
