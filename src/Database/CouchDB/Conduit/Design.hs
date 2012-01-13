{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-} 

-- | High-level API for CouchDB design documents. These methods are very 
--   convenient for bootstrapping and testing.

module Database.CouchDB.Conduit.Design (
    couchViewPut
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

import Database.CouchDB.Conduit (MonadCouch, CouchError, Path, mkPath, Revision)
import Database.CouchDB.Conduit.Internal.Doc (couchGetWith, couchPutWith)

-- | Put view in design document. If design document does not exist, it 
--   will be created. 
couchViewPut :: MonadCouch m =>
       Path                 -- ^ Design document
    -> Path                 -- ^ View name
    -> B.ByteString         -- ^ Map function
    -> Maybe B.ByteString   -- ^ Reduce function
    -> ResourceT m Revision
couchViewPut designName viewName mapF reduceF = do
    -- Get design or empty object
    (rev, A.Object d) <- catch 
        (couchGetWith A.Success path [])
        (\(_ :: CouchError) -> return (B.empty, AT.emptyObject))
    couchPutWith A.encode path rev [] $ inferViews (purge_ d)
  where
    path = (mkPath ["_design", designName])
    inferViews d = A.Object $ M.insert "views" (addView d) d
    addView d = A.Object $ M.insert 
        (TE.decodeUtf8 viewName)
        (constructView mapF reduceF) 
        (extractViews d)
    constructView :: B.ByteString -> Maybe B.ByteString -> A.Value
    constructView m (Just r) = A.object ["map" A..= m, "reduce" A..= r]
    constructView m Nothing = A.object ["map" A..= m]

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


