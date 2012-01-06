{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.Conduit.Internal.Parser where

import qualified    Data.Text.Encoding as TE
import qualified    Data.HashMap.Lazy as M
import qualified    Data.Aeson as A

import              Database.CouchDB.Conduit

valToRev :: A.Value -> Either CouchError Revision
valToRev (A.Object o) = case M.lookup "rev" o of
    (Just (A.String r)) -> Right $ TE.encodeUtf8 r
    _  -> Left $ CouchError Nothing "unable to find revision"  
valToRev _ = Left $ CouchError Nothing "Couch DB did not return an object"
