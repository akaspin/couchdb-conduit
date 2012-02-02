{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-} 

-- | High-level API for CouchDB design documents. These methods are very 
--   convenient for bootstrapping and testing.

module Database.CouchDB.Conduit.Design (
    couchPutView_,
    couchPutView'
) where

import              Prelude hiding (catch)
import              Control.Exception.Lifted (catch)

import              Data.Conduit (ResourceT)

import qualified    Data.ByteString as B
import qualified    Data.Text as T
import qualified    Data.Text.Encoding as TE
import qualified    Data.HashMap.Lazy as M
import qualified    Data.Aeson as A
import qualified    Data.Aeson.Types as AT

import Database.CouchDB.Conduit.Internal.Connection 
        (MonadCouch, CouchError, Path, mkPath, Revision)
import Database.CouchDB.Conduit.Internal.Doc (couchGetWith, 
            couchPutWith_, couchPutWith')

-- | Put view in design document if it not exists. If design document does 
--   not exist, it will be created. 
couchPutView_ :: MonadCouch m =>
       Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> B.ByteString         -- ^ Map function
    -> Maybe B.ByteString   -- ^ Reduce function
    -> ResourceT m Revision
couchPutView_ = couchViewPutInt True

-- | Brute-force version of 'couchViewPut''. Put view in design document. 
--   If design document does not exist, it will be created. 
couchPutView' :: MonadCouch m =>
       Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> B.ByteString         -- ^ Map function
    -> Maybe B.ByteString   -- ^ Reduce function
    -> ResourceT m Revision
couchPutView' = couchViewPutInt False

-----------------------------------------------------------------------------
-- Internal
-----------------------------------------------------------------------------

couchViewPutInt :: MonadCouch m =>
       Bool
    -> Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> B.ByteString         -- ^ Map function
    -> Maybe B.ByteString   -- ^ Reduce function
    -> ResourceT m Revision
couchViewPutInt prot designName viewName mapF reduceF = do
    -- Get design or empty object
    (_, A.Object d) <- getDesignDoc path
    if prot then couchPutWith_ A.encode path [] $ inferViews (purge_ d)
            else couchPutWith' A.encode path [] $ inferViews (purge_ d)
  where
    path = designDocPath designName
    inferViews d = A.Object $ M.insert "views" (addView d) d
    addView d = A.Object $ M.insert 
        (TE.decodeUtf8 viewName)
        (constructView mapF reduceF) 
        (extractViews d)
    constructView :: B.ByteString -> Maybe B.ByteString -> A.Value
    constructView m (Just r) = A.object ["map" A..= m, "reduce" A..= r]
    constructView m Nothing = A.object ["map" A..= m]

getDesignDoc :: MonadCouch m => Path -> ResourceT m (Revision, AT.Value)
getDesignDoc designName = catch 
        (couchGetWith A.Success (designDocPath designName) [])
        (\(_ :: CouchError) -> return (B.empty, AT.emptyObject))
    
designDocPath :: Path -> Path
designDocPath dn = mkPath ["_design", dn]

-- | Purge underscore fields
purge_ :: AT.Object -> AT.Object
purge_ = M.filterWithKey (\k _ -> k `notElem` ["_id", "_rev"])

-- | Strip 'A.Value'
stripObject :: AT.Value -> AT.Object
stripObject (A.Object a) = a 
stripObject _ = M.empty 

-- Extract views field or return empty map
extractViews :: M.HashMap T.Text AT.Value -> M.HashMap T.Text AT.Value
extractViews o = maybe M.empty stripObject $ M.lookup "views" o


