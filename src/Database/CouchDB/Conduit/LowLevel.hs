{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DoAndIfThenElse #-}

-- | Low-level method and tools of accessing CouchDB.

module Database.CouchDB.Conduit.LowLevel (
    CouchResponse,
    couch,
    protect,
    protect'
) where

import              Prelude hiding (catch)

import Control.Exception.Lifted (catch)
import Control.Exception (SomeException)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Base (liftBase)

import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as M
import Data.String.Conversions ((<>), cs)

import Data.Conduit (ResourceT, Source, 
                        ($$), resourceThrow)
import Data.Conduit.Attoparsec (sinkParser)
                        
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit.Internal.Connection

-- | CouchDB response
type CouchResponse m = H.Response (Source m B.ByteString)

-- | The most general method of accessing CouchDB.  This is a very thin wrapper 
--   around 'H.http'.  Most of the time you should use one of the other access 
--   functions, but this function is needed for example to write and read 
--   attachments that are not in JSON format.
couch :: MonadCouch m =>
       HT.Method                -- ^ Method
    -> Path                     -- ^ Correct 'Path' with escaped fragments.
    -> HT.RequestHeaders        -- ^ Headers
    -> HT.Query                 -- ^ Query args
    -> H.RequestBody m          -- ^ Request body
    -> (CouchResponse m -> ResourceT m (CouchResponse m))
                                -- ^ Protect function. See 'protect'
    -> ResourceT m (CouchResponse m)
couch meth path hdrs qs reqBody protectFn = do
    conn <- lift couchConnection
    let req = H.def 
            { H.method          = meth
            , H.host            = couchHost conn
            , H.requestHeaders  = hdrs
            , H.port            = couchPort conn
            , H.path            = withPrefix $ couchPrefix conn
            , H.queryString     = HT.renderQuery False qs
            , H.requestBody     = reqBody
            , H.checkStatus = const . const $ Nothing }
    -- Apply auth if needed
    let req' = if couchLogin conn == B.empty then req else H.applyBasicAuth 
            (couchLogin conn) (couchPass conn) req
    res <- H.http req' (fromJust $ couchManager conn)
    protectFn res
  where
    withPrefix prx 
        | B.null prx = path
        | otherwise = "/" <> prx <> B.tail path 

-- | Protect 'H.Response' from bad status codes. If status code in list 
--   of status codes - just return response. Otherwise - throw 'CouchError'.
--   
--   Instead 'H.checkStatus', 'protect' parses CouchDB response body JSON and
--   extract \"reason\" message.
--   
--   To protect from typical errors use 'protect''.
protect :: MonadCouch m => 
       [Int]             -- ^ Good codes
    -> (CouchResponse m -> ResourceT m (CouchResponse m)) -- ^ handler
    -> CouchResponse m   -- ^ Response
    -> ResourceT m (CouchResponse m)
protect goodCodes h ~resp@(H.Response (HT.Status sc sm) _ bsrc)
    | sc == 304 = liftBase $ resourceThrow NotModified
    | sc `elem` goodCodes = h resp
    | otherwise = do
        v <- catch (bsrc $$ sinkParser A.json)
                   (\(_::SomeException) -> return A.Null)
        liftBase $ resourceThrow $ CouchHttpError sc $ msg v
        where 
        msg v = sm <> reason v
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
    -> ResourceT m (CouchResponse m)
protect' = protect [200, 201, 202, 304] return
