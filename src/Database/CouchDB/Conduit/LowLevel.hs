{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Low-level method and tools of accessing CouchDB.

module Database.CouchDB.Conduit.LowLevel (
    CouchResponse,
    couch,
    protect,
    protect'
) where

import              Prelude hiding (catch)
import              Control.Exception.Lifted (catch)

import              Control.Exception (SomeException)
import              Control.Monad.Trans.Class (lift)
import              Control.Monad.Base (liftBase)

import              Data.Maybe (fromJust)
import qualified    Data.ByteString as B
import              Data.Aeson (json, Value(..))
import qualified    Data.ByteString.UTF8 as BU8
import qualified    Data.HashMap.Lazy as M
import qualified    Data.Text as T

import              Data.Conduit (ResourceT, BufferedSource, 
                        ($$), resourceThrow)
import              Data.Conduit.Attoparsec (sinkParser)
                        
import qualified    Network.HTTP.Conduit as H
import qualified    Network.HTTP.Types as HT

import              Database.CouchDB.Conduit

-- | CouchDB response
type CouchResponse m = H.Response (BufferedSource m B.ByteString)

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
    -> CouchResponse m   -- ^ Response
    -> ResourceT m (CouchResponse m)
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
       CouchResponse m   -- ^ Response
    -> ResourceT m (CouchResponse m)
protect' = protect [200, 201, 202, 304]
