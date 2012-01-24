
module Database.CouchDB.Conduit.Internal.View where

import qualified    Data.Aeson as A
import              Data.Conduit (resourceThrow, Conduit(..), ResourceIO)
import qualified    Data.Conduit.List as CL (mapM)
import Data.ByteString.Char8 (pack)

import              Database.CouchDB.Conduit

-- | Convert CouchDB view row or row value from 'Database.CouchDB.Conduit.View' 
--   to concrete type.
--   
-- > res <- couchView "mydesign" "myview" [] $ rowValue =$= toType =$ consume
toTypeWith :: ResourceIO m =>
    (A.Value -> A.Result a)       -- ^ Parser 
    -> Conduit A.Value m a
toTypeWith f = CL.mapM (\v -> case f v of
            A.Error e -> resourceThrow $ CouchInternalError $
                           pack ("Error parsing json: " ++ e)
            A.Success o -> return o)
