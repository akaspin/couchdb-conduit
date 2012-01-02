{-# LANGUAGE DeriveDataTypeable #-} 

module Database.CouchDB.Conduit.Error (
    CouchError(..)
) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

-- | A Couch DB Error. If the error comes from http, the http status code 
--   is also given. Non-http errors include things like errors  
--   parsing the response.
data CouchError = CouchError (Maybe Int) String
    deriving (Show, Typeable)
instance Exception CouchError