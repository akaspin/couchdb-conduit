{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.Conduit.Internal.Parser where

import Data.Conduit
import qualified    Data.Text as T
import qualified    Data.Text.Encoding as TE
import qualified    Data.HashMap.Lazy as M
import qualified    Data.Aeson as A

import              Database.CouchDB.Conduit

valToRev :: A.Value -> Either CouchError Revision
valToRev (A.Object o) = case M.lookup "rev" o of
    (Just (A.String r)) -> Right $ TE.encodeUtf8 r
    _  -> Left $ CouchError Nothing "unable to find revision"  
valToRev _ = Left $ CouchError Nothing "Couch DB did not return an object"


--extractField :: String -> A.Value -> Either CouchError Revision
extractField ::ResourceIO m =>
                               T.Text -> A.Value -> m A.Value
extractField s (A.Object o) = case M.lookup s o of
    (Just r) -> return r
    _  -> resourceThrow $ CouchError Nothing "unable to find field"  
extractField _ _ = resourceThrow $ CouchError Nothing 
        "Couch DB did not return an object"

