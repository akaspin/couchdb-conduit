
module Database.CouchDB.Conduit.Explicit (
    couchGet
) where


import qualified Data.ByteString as B
import qualified Data.Aeson as A

import Data.Conduit (ResourceIO, runResourceT, ($$), resourceThrow)
import Data.Conduit.Attoparsec (sinkParser)

import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types as HT

import Database.CouchDB.Conduit

-- | Load a single object from couch DB.
couchGet :: (MonadCouch m) => 
       DocPath      -- ^ Document path
    -> HT.Query     -- ^ Query
    -> m A.Object
couchGet p q = do
    res <- runResourceT $ couch HT.methodGet p [] q 
            (protect syncJSON) 
            (H.RequestBodyBS B.empty)
    either resourceThrow return $ valToObj res

-- | Basic consumer for json            
syncJSON :: ResourceIO m => H.ResponseConsumer m A.Value
syncJSON _status _hdrs bsrc = bsrc $$ sinkParser A.json

-- | Convers a value to an object
valToObj :: A.Value -> Either CouchError A.Object
valToObj (A.Object o) = Right o
valToObj _ = Left $ CouchError Nothing "Couch DB did not return an object"