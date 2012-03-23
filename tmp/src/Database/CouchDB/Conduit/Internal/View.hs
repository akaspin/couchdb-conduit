{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.Conduit.Internal.View where

import Control.Exception.Lifted (throw)

import qualified Data.Aeson as A
import Data.Conduit (Conduit(..), MonadResource)
import qualified Data.Conduit.List as CL (mapM)
import Data.String.Conversions ((<>), cs)

import Database.CouchDB.Conduit.Internal.Connection (CouchError(..))

-- | Convert CouchDB view row or row value from 'Database.CouchDB.Conduit.View' 
--   to concrete type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toTypeWith :: MonadResource m =>
    (A.Value -> A.Result a)       -- ^ Parser 
    -> Conduit A.Value m a
toTypeWith f = CL.mapM (\v -> case f v of
            A.Error e -> throw $ CouchInternalError $
                           "Error parsing json: " <> cs e
            A.Success o -> return o)
