{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.Conduit.Internal.View where

import qualified Data.Aeson as A
import Data.Conduit (resourceThrow, Conduit(..), ResourceIO)
import qualified Data.Conduit.List as CL (mapM)
import Data.String.Conversions ((<>), cs)

import Database.CouchDB.Conduit.Internal.Connection (CouchError(..))

-- | Convert CouchDB view row or row value from 'Database.CouchDB.Conduit.View' 
--   to concrete type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toTypeWith :: ResourceIO m =>
    (A.Value -> A.Result a)       -- ^ Parser 
    -> Conduit A.Value m a
toTypeWith f = CL.mapM (\v -> case f v of
            A.Error e -> resourceThrow $ CouchInternalError $
                           "Error parsing json: " <> cs e
            A.Success o -> return o)
