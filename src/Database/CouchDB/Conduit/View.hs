{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-} 

-- | Higher-level functions to interact with CouchDB views.
--   To manipulate views in design documents see 
--   "Database.CouchDB.Conduit.Design"

module Database.CouchDB.Conduit.View 
(   
    -- * Acccessing views #run#
    -- $run
    couchView,
    couchView',
    couchViewPost,
    couchViewPost',
    rowValue,

    -- * View query parameters
    -- $view_query #view_query#
    mkParam 
)
 where

import Control.Monad.Trans.Class (lift)
import Control.Applicative ((<|>))

import Data.Monoid (mconcat)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC8
import qualified Data.HashMap.Lazy as M
import qualified Data.Aeson as A
import Data.Attoparsec

import Data.Conduit (ResourceIO, ResourceT, 
                        Source, Conduit, Sink, ($$), ($=), 
                        sequenceSink, SequencedSinkResponse(..),
                        resourceThrow )
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Attoparsec as CA

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit.Internal.Connection
import Database.CouchDB.Conduit.LowLevel (couch, protect')

-----------------------------------------------------------------------------
-- View query parameters
-----------------------------------------------------------------------------

-- $view_query
-- For details see 
-- <http://wiki.apache.org/couchdb/HTTP_view_API#Querying_Options>. Note, 
-- because all options must be a proper URL encoded JSON, construction of 
-- complex parameters can be very tedious. To simplify this, use 'mkParam'. 

-- | Encode query parameter to 'B.ByteString'.  
--
-- > mkParam (["a", "b"] :: [String])
-- > "[\"a\",\"b\"]"
--
-- It't just convert lazy 'BL.ByteString' from 'A.encode' to strict 
-- 'B.ByteString'
mkParam :: A.ToJSON a =>
       a                -- ^ Parameter
    -> B.ByteString
mkParam = mconcat . BL.toChunks . A.encode

-----------------------------------------------------------------------------
-- Running
-----------------------------------------------------------------------------

-- $run
-- In contrast to the functions of access to documents that are loaded into 
-- memory entirely. 'couchView' and 'couchView'' combines the incredible power 
-- of /http-conduit/ and /attoparsec/ to allow you to process objects in 
-- constant space.  
--
-- As data is read from the network, it is fed into attoparsec. When 
-- attoparsec completes parsing row, it sent to 'Sink'. 'Sink' can be composed 
-- from many conduits with sink at the end, such as 'rowValue', view conduits 
-- from "Database.CouchDB.Conduit.Explicit#view" and 
-- "Database.CouchDB.Conduit.Generic#view", and many others. See 
-- "Data.Conduit" for details and documentation.

-- | Run CouchDB view in manner like 'H.http'.
--
-- > runCouch def $ do
-- >
-- >     -- Print all upon receipt.
-- >     src <- couchView "mydb" "mydesign" "myview" [] 
-- >     src $$ CL.mapM_ (liftIO . print)
-- >
-- >     -- ... Or extract row value and consume
-- >     src' <- couchView "mydb" "mydesign" "myview" [] 
-- >     res <- src' $= rowValue $$ CL.consume
couchView :: MonadCouch m =>
       Path                 -- ^ Database
    -> Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> HT.Query             -- ^ Query parameters
    -> ResourceT m (Source m A.Object)
couchView db design view q = do
    H.Response _ _ bsrc <- couch HT.methodGet 
            (viewPath db design view)
            [] q 
            (H.RequestBodyBS B.empty) protect'
    return $ bsrc $= conduitCouchView

-- | Brain-free version of 'couchView'. Takes 'Sink' to consume response.
--
-- > runCouch def $ do
-- >
-- >     -- Print all upon receipt.
-- >     couchView' "mydb" "mydesign" "myview" [] $ CL.mapM_ (liftIO . print)
-- >
-- >     -- ... Or extract row value and consume
-- >     res <- couchView' "mydb" "mydesign" "myview" [] $ 
-- >                        rowValue =$ CL.consume
couchView' :: MonadCouch m =>
       Path                 -- ^ Database
    -> Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> HT.Query             -- ^ Query parameters
    -> Sink A.Object m a    -- ^ Sink for handle view rows.
    -> ResourceT m a
couchView' db design view q sink = do
    raw <- couchView db design view q
    raw $$ sink

-- | Run CouchDB view in manner like 'H.http' using @POST@ (since CouchDB 0.9).
--   It's convenient in case that @keys@ paremeter too big for @GET@ query 
--   string. Other query parameters used as usual.
-- 
-- > runCouch def $ do
-- >     src <- couchViewPost "mydb" "mydesign" "myview" 
-- >             [("group", Just "true")]
-- >             ["key1", "key2", "key3"] 
-- >     src $$ CL.mapM_ (liftIO . print)
couchViewPost :: (MonadCouch m, A.ToJSON a) =>
       Path                 -- ^ Database
    -> Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> HT.Query             -- ^ Query parameters
    -> a                    -- ^ View @keys@. Must be list or cortege.
    -> ResourceT m (Source m A.Object)    
couchViewPost db design view q ks = do
    H.Response _ _ bsrc <- couch HT.methodPost 
            (viewPath db design view)  
            [] 
            q 
            (H.RequestBodyLBS mkPost) protect'
    return $ bsrc $= conduitCouchView
  where
    mkPost = A.encode $ A.object ["keys" A..= ks]

-- | Brain-free version of 'couchViewPost'. Takes 'Sink' to consume response.
couchViewPost' :: (MonadCouch m, A.ToJSON a) =>
       Path                 -- ^ Database
    -> Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> HT.Query             -- ^ Query parameters
    -> a                    -- ^ View @keys@. Must be list or cortege.
    -> Sink A.Object m a    -- ^ Sink for handle view rows.
    -> ResourceT m a
couchViewPost' db design view q ks sink = do
    raw <- couchViewPost db design view q ks
    raw $$ sink

-- | Conduit for extract \"value\" field from CouchDB view row.
rowValue :: ResourceIO m => Conduit A.Object m A.Value
rowValue = CL.mapM (\v -> case M.lookup "value" v of
            (Just o) -> return o
            _ -> resourceThrow $ CouchInternalError $ BC8.pack
                    ("View row does not contain value: " ++ show v))

-----------------------------------------------------------------------------
-- Internal
-----------------------------------------------------------------------------

-- | Make full view path
viewPath :: Path -> Path -> Path -> Path
viewPath db design view = mkPath [db, "_design", design, "_view", view]

-----------------------------------------------------------------------------
-- Internal view parser
-----------------------------------------------------------------------------

conduitCouchView :: ResourceIO m => Conduit B.ByteString m A.Object
conduitCouchView = sequenceSink () $ \() -> do
    b <- CA.sinkParser viewStart
    if b then return $ StartConduit viewLoop
         else return Stop

viewLoop :: ResourceIO m => Conduit B.ByteString m A.Object   
viewLoop = sequenceSink False $ \isLast -> 
    if isLast then return Stop
    else do 
        v <- CA.sinkParser (A.json <?> "json object")
        vobj <- case v of
            (A.Object o) -> return o
            _ -> lift $ resourceThrow $ 
                 CouchInternalError "view entry is not an object"
        res <- CA.sinkParser (commaOrClose <?> "comma or close")
        case res of
            Comma -> return $ Emit False [vobj]
            CloseBracket -> return $ Emit True [vobj]

data CommaOrCloseBracket = Comma | CloseBracket

commaOrClose :: Parser CommaOrCloseBracket
commaOrClose = do
    skipWhile (\c -> c /= 44 && c /= 93) <?> 
            "Checking for next comma"
    w <- anyWord8
    if w == 44 then return Comma else return CloseBracket

-- determine view
viewStart :: Parser Bool
viewStart = do
    _ <- string "{" 
    _ <- option "" $ string "\"total_rows\":" 
    option () $ skipWhile (\x -> x >= 48 && x <= 57)
    _ <- option "" $ string ",\"update_seq\":" 
    option () $ skipWhile (\x -> x >= 48 && x <= 57)
    _ <- option "" $ string ",\"offset\":"
    option () $ skipWhile (\x -> x >= 48 && x <= 57)
    _ <- option "" $ string ","
    _ <- string "\"rows\":["
    (string "]}" <|> (do
        r <- string "]"
        _ <- option "" $ string ",\"update_seq\":"
        option () $ skipWhile (\x -> x >= 48 && x <= 57)
        _ <- option "" $ string "}"
        return r) 
        >> return False) <|> return True

    
    
    
    
    