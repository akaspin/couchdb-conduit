-- | Internal
module Database.CouchDB.Conduit.Internal (
    sinkZero,
    sinkJSON
) where

import Data.Conduit (ResourceIO, ($$))
import Data.Conduit.List (sinkNull)
import Data.Conduit.Attoparsec (sinkParser)

import Network.HTTP.Conduit (ResponseConsumer)

import Data.Aeson (Value, json)

sinkZero :: ResourceIO m => ResponseConsumer m ()
sinkZero _status _hdrs bsrc = bsrc $$ sinkNull

-- | Basic consumer for json            
sinkJSON :: ResourceIO m => ResponseConsumer m Value
sinkJSON _status _hdrs bsrc = bsrc $$ sinkParser json