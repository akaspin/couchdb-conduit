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
    viewQp,
    viewQpBS,
    viewQpInt,
    viewQpTrue,
    viewQpFalse,
    
    -- ** Rows
    -- $view_query_rows #view_query_rows#
    viewQpDescending,
    viewQpLimit,
    viewQpSkip,
    viewQpStartId,
    viewQpEndId,

    -- ** Map/Reduce
    -- $view_query_reduce #view_query_reduce#
    viewQpGroup,
    viewQpGroupLevel,
    viewQpReduceOff,
    viewQpReduceOn,
    
    -- ** Keys
    -- $view_query_keys #view_query_keys#
    viewQpKey,
    viewQpKeys,

    -- ** Control
    -- $view_query_control #view_query_control#
    viewQpIncludeDocs,
    viewQpInclusiveEnd
)
 where

import Control.Applicative ((<|>))
import Control.Exception.Lifted (throw)

import Data.Monoid (mconcat)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Lazy as M
import qualified Data.Aeson as A
import Data.Attoparsec

import Data.Conduit (MonadResource, 
                        Source, Conduit, Sink, ($$), ($=), 
                        sequenceSink, SequencedSinkResponse(..),
                        )
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
-- complex parameters can be very tedious. To simplify this, use following
-- functions. 

-- | Make complex view query parameter.  
--
-- > viewQP "key" (["a", "b"] :: [String])
-- > ("key", Just "[\"a\",\"b\"]")
--
-- It't just convert lazy 'BL.ByteString' from 'A.encode' to strict 
-- 'B.ByteString'. For more efficient use specific functions. 
viewQp :: A.ToJSON a =>
       B.ByteString     -- ^ Query parameter name
    -> a                -- ^ Parameter
    -> HT.QueryItem
viewQp n v = (n, Just $ mconcat . BL.toChunks . A.encode $ v)

-- | Make quoted 'B.ByteString' query parameter.
viewQpBS :: B.ByteString -> B.ByteString -> HT.QueryItem
viewQpBS n v = (n, Just $ "\"" `B.append` v `B.append` "\"")

-- | Make 'Int' query parameter.
viewQpInt :: B.ByteString -> Int -> HT.QueryItem
viewQpInt n v = (n, Just $ BS8.pack . show $ v)

-- | Make @...=true@ query parameter.
viewQpTrue :: B.ByteString -> HT.QueryItem
viewQpTrue n = (n, Just "true")

-- | Make @...=true@ query parameter.
viewQpFalse :: B.ByteString -> HT.QueryItem
viewQpFalse n = (n, Just "false")

-- $view_query_rows
-- Helpers for sorting and limiting rows. 

-- $view_query_reduce
-- Helpers for Map/Reduce. 

-- $view_query_keys
-- Helpers for quering by keys. 

-- $view_query_control
-- Helpers for view behaviour. 

---------------------------
-- Boolean query parameters
---------------------------

-- | Turn on descending sort of view results. 
--   Shorthand for @viewQpTrue \"descending\"@.
viewQpDescending :: HT.QueryItem
viewQpDescending = viewQpTrue "descending"

-- | Turn on grouping.
--   Shorthand for @viewQpTrue \"group\"@.
viewQpGroup :: HT.QueryItem
viewQpGroup = viewQpTrue "group"

-- | Turn on inclusion docs in view results.
--   Shorthand for @viewQpTrue \"include_docs\"@.
viewQpIncludeDocs :: HT.QueryItem
viewQpIncludeDocs = viewQpTrue "include_docs"

-- | Turn off inclusion @endkey@ in view results.
--   Shorthand for @viewQpFalse \"inclusive_end\"@.
viewQpInclusiveEnd :: HT.QueryItem
viewQpInclusiveEnd = viewQpFalse "inclusive_end"

-- | Force off reduce if a reduce function is defined.
--   Shorthand for @viewQpFalse \"reduce\"@.
viewQpReduceOff :: HT.QueryItem
viewQpReduceOff = viewQpFalse "reduce"

-- | Force on reduce if a reduce function is not defined.
--   Shorthand for @viewQpTrue \"reduce\"@.
viewQpReduceOn :: HT.QueryItem
viewQpReduceOn = viewQpTrue "reduce"

--------------
-- Pure string
--------------

-- | Document id to start with.
--   Shorthand for @viewQpBS \"startkey_docid\"@.
viewQpStartId :: 
       Path             -- ^ Document ID.
    -> HT.QueryItem
viewQpStartId = viewQpBS "startkey_docid"

-- | Last document id to include in the output.
--   Shorthand for @viewQpBS \"endkey_docid\"@.
viewQpEndId :: 
       Path             -- ^ Document ID.
    -> HT.QueryItem
viewQpEndId = viewQpBS "endkey_docid"

--------------
-- Numeric
--------------

-- | Limit view rows. 
--   Shorthand for @viewQpInt \"limit\"@.
viewQpLimit :: 
       Int              -- ^ Max number of rows.
    -> HT.QueryItem
viewQpLimit = viewQpInt "limit"

-- | Skip view rows. 
--   Shorthand for @viewQpInt \"skip\"@.
viewQpSkip :: 
       Int              -- ^ Number of rows to skip. 
    -> HT.QueryItem
viewQpSkip = viewQpInt "skip"

-- | Set grouping level. 
--   Shorthand for @viewQpInt \"group_level\"@.
viewQpGroupLevel :: 
       Int          -- ^ Grouping level. 
    -> HT.QueryItem
viewQpGroupLevel = viewQpInt "group_level"

--------------
-- Complex
--------------

-- | Make @key=...@ query parameter.
--   Shorthand for @viewQp \"key\"@.
viewQpKey :: A.ToJSON a => 
       a                -- ^ Key 
    -> HT.QueryItem
viewQpKey = viewQp "key"

-- | Make @keys=...@ query parameter. 
--   Shorthand for @viewQp \"keys\"@.
--
--   Use it only with 'couchView' and 'couchView''. For large sets of @keys@
--   use 'couchViewPost' and 'couchViewPost''
viewQpKeys :: A.ToJSON a => 
       a                -- ^ Keys. Must be list or cortege. 
    -> HT.QueryItem
viewQpKeys = viewQp "keys"

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
    -> m (Source m A.Object)
couchView db design view q = do
    H.Response _ _ _ bsrc <- couch HT.methodGet 
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
    -> m a
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
    -> m (Source m A.Object)    
couchViewPost db design view q ks = do
    H.Response _ _ _ bsrc <- couch HT.methodPost 
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
    -> m a
couchViewPost' db design view q ks sink = do
    raw <- couchViewPost db design view q ks
    raw $$ sink

-- | Conduit for extract \"value\" field from CouchDB view row.
rowValue :: Monad m => Conduit A.Object m A.Value
rowValue = CL.mapM (\v -> case M.lookup "value" v of
            (Just o) -> return o
            _ -> throw $ CouchInternalError $ BS8.pack
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

conduitCouchView :: MonadResource m => Conduit B.ByteString m A.Object
conduitCouchView = sequenceSink () $ \() -> do
    b <- CA.sinkParser viewStart
    if b then return $ StartConduit viewLoop
         else return Stop

viewLoop :: MonadResource m => Conduit B.ByteString m A.Object   
viewLoop = sequenceSink False $ \isLast -> 
    if isLast then return Stop
    else do 
        v <- CA.sinkParser (A.json <?> "json object")
        vobj <- case v of
            (A.Object o) -> return o
            _ -> throw $ 
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

    
    
    
    
    