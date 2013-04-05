{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, NoMonomorphismRestriction #-} 

-- | Higher-level functions to interact with CouchDB views.
--   
--   To automate creation of CouchDB Query Options see
--   "Database.CouchDB.Conduit.View.Query"
--
--   To manipulate views in design documents see 
--   "Database.CouchDB.Conduit.Design"

module Database.CouchDB.Conduit.View 
(   
    -- * Acccessing views #run#
    -- $run
    couchView,
    couchView_,
    couchViewPost,
    couchViewPost_,
    rowValue,
    rowDoc,
    rowField,
) where

import Control.Exception.Lifted (throw)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M
import qualified Data.Aeson as A
import Data.Attoparsec

import qualified Data.Vector.Generic as V

import Data.Conduit (MonadResource, Source, Conduit, Sink, ($$), ($=), ($$+-), yield)
                     
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Attoparsec as CA

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit.Internal.Connection
import Database.CouchDB.Conduit.LowLevel (couch, protect')

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
    resp <- couch HT.methodGet 
            (viewPath db design view)
            [] q 
            (H.RequestBodyBS B.empty) protect'
    H.responseBody resp $$+- conduitRows

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
couchView_ :: MonadCouch m =>
       Path                 -- ^ Database
    -> Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> HT.Query             -- ^ Query parameters
    -> Sink A.Object m a    -- ^ Sink for handle view rows.
    -> m a
couchView_ db design view q sink = do
    raw <- couchView db design view q
    raw $$ sink

-- | Run CouchDB view in manner like 'H.http' using @POST@ (since CouchDB 0.9).
--   It's convenient in case that @keys@ paremeter too big for @GET@ query 
--   string. Other query parameters used as usual.
-- 
-- > runCouch def $ do
-- >     src <- couchViewPost "mydb" "mydesign" "myview" 
-- >             (mkQuery [QPGroup])
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
    resp <- couch HT.methodPost 
            (viewPath db design view)  
            [] 
            q 
            (H.RequestBodyLBS mkPost) protect'
    H.responseBody resp $$+- conduitRows
  where
    mkPost = A.encode $ A.object ["keys" A..= ks]

-- | Brain-free version of 'couchViewPost'. Takes 'Sink' to consume response.
couchViewPost_ :: (MonadCouch m, A.ToJSON a) =>
       Path                 -- ^ Database
    -> Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> HT.Query             -- ^ Query parameters
    -> a                    -- ^ View @keys@. Must be list or cortege.
    -> Sink A.Object m a    -- ^ Sink for handle view rows.
    -> m a
couchViewPost_ db design view q ks sink = do
    raw <- couchViewPost db design view q ks
    raw $$ sink

-- | Conduit for extract \"value\" field from CouchDB view row.
rowValue :: Monad m => Conduit A.Object m A.Value
rowValue = rowField "value"

-- | Conduit for extract \"doc\" field from CouchDB view row. 
--   Use only with @include_docs=true@ query parameter.
rowDoc :: Monad m => Conduit A.Object m A.Value
rowDoc = rowField "doc"

-- | Extract field from view row
rowField :: Monad m => T.Text -> Conduit A.Object m A.Value
rowField f = CL.mapMaybe (M.lookup f) 

-----------------------------------------------------------------------------
-- Internal
-----------------------------------------------------------------------------

-- | Make full view path
viewPath :: Path -> Path -> Path -> Path
viewPath db design view = mkPath [db, "_design", design, "_view", view]

-- | Use an immutable vector as a source.
sourceVector :: (Monad m, V.Vector v a) => v a -> Source m a
sourceVector = V.mapM_ yield

-- | Extra
conduitRows :: MonadResource m => Sink BS8.ByteString m (Source m A.Object)
conduitRows = do 
    raw <- CA.sinkParser (A.json <?> "json object")
    rows <- case raw of
        (A.Object raw') -> case M.lookup "rows" raw' of
            (Just (A.Array r)) -> return r
            _ -> return V.empty
        _ -> throw $ CouchInternalError "view entry is not an object"
    return $ sourceVector rows $= CL.map valToObj
  where
    valToObj (A.Object o) = o
    valToObj _ = throw $ CouchInternalError "row is not object"

    
    
    
    
    