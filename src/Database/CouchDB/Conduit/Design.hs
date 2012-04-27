{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-} 

-- | High-level API for CouchDB design documents. These methods are very 
--   convenient for bootstrapping and testing.

module Database.CouchDB.Conduit.Design (
    couchPutView
) where

import Prelude hiding (catch)
import Control.Monad (void)
import Control.Exception.Lifted (catch)

import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Lazy as M
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

import Database.CouchDB.Conduit.Internal.Connection 
        (MonadCouch, CouchError, Path, mkPath, Revision)
import Database.CouchDB.Conduit.Internal.Doc (couchGetWith, couchPutWith')

-- | Put view to design document. If design document does not exist, 
--   it will be created. 
couchPutView :: MonadCouch m =>
       Path                 -- ^ Database
    -> Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> B.ByteString         -- ^ Map function
    -> Maybe B.ByteString   -- ^ Reduce function
    -> m ()
couchPutView db designName viewName mapF reduceF = do
    (_, A.Object d) <- getDesignDoc path
    void $ couchPutWith' A.encode path [] $ inferViews (purge_ d)
  where
    path = designDocPath db designName
    inferViews d = A.Object $ M.insert "views" (addView d) d
    addView d = A.Object $ M.insert 
        (TE.decodeUtf8 viewName)
        (constructView mapF reduceF) 
        (extractViews d)
    constructView :: B.ByteString -> Maybe B.ByteString -> A.Value
    constructView m (Just r) = A.object ["map" A..= m, "reduce" A..= r]
    constructView m Nothing = A.object ["map" A..= m]

-----------------------------------------------------------------------------
-- Internal
-----------------------------------------------------------------------------

getDesignDoc :: MonadCouch m => 
       Path 
    -> m (Revision, AT.Value)
getDesignDoc designName = catch 
        (couchGetWith A.Success designName [])
        (\(_ :: CouchError) -> return (B.empty, AT.emptyObject))
    
designDocPath :: Path -> Path -> Path
designDocPath db dn = mkPath [db, "_design", dn]

-- | Purge underscore fields
purge_ :: AT.Object -> AT.Object
purge_ = M.filterWithKey (\k _ -> k `notElem` ["_id", "_rev"])

-- | Strip 'A.Value'
stripObject :: AT.Value -> AT.Object
stripObject (A.Object a) = a 
stripObject _ = M.empty 

-- Extract views field or return empty map
extractViews :: M.HashMap Text AT.Value -> M.HashMap Text AT.Value
extractViews o = maybe M.empty stripObject $ M.lookup "views" o


