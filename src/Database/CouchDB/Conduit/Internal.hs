{-# LANGUAGE OverloadedStrings #-}

-- | Internal
module Database.CouchDB.Conduit.Internal (
    valToRev,
    sinkZero,
    sinkJSON
) where

import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Lazy as M
import Data.Conduit (ResourceIO, ($$))
import Data.Conduit.List (sinkNull)
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Aeson as A
import Network.HTTP.Conduit (ResponseConsumer)
import Database.CouchDB.Conduit (CouchError(..), Revision)


valToRev :: A.Value -> Either CouchError Revision
valToRev (A.Object o) = case M.lookup "rev" o of
    (Just (A.String r)) -> Right $ TE.encodeUtf8 r
    _  -> Left $ CouchError Nothing "unable to find revision"  
valToRev _ = Left $ CouchError Nothing "Couch DB did not return an object"

-----------------------------------------------------------------------------
-- Sinks
-----------------------------------------------------------------------------

sinkZero :: ResourceIO m => ResponseConsumer m ()
sinkZero _status _hdrs bsrc = bsrc $$ sinkNull

-- | Basic consumer for json            
sinkJSON :: ResourceIO m => ResponseConsumer m A.Value
sinkJSON _status _hdrs bsrc = bsrc $$ sinkParser A.json