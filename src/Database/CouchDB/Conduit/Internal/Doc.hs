{-# LANGUAGE OverloadedStrings #-}

-- | Internal
module Database.CouchDB.Conduit.Internal.Doc (
    couchRev,
    couchDelete
) where

import              Data.Maybe (fromJust)

import qualified    Data.ByteString as B

import              Data.Conduit (runResourceT)
import qualified    Network.HTTP.Conduit as H
import              Network.HTTP.Types as HT

import              Database.CouchDB.Conduit

------------------------------------------------------------------------------
-- Type-independent methods
------------------------------------------------------------------------------

-- | Get Revision of a document. 
couchRev :: MonadCouch m => 
       DocPath 
    -> m Revision
couchRev p = runResourceT $ do
    (H.Response _ hs _) <- couch HT.methodHead p [] [] 
            (H.RequestBodyBS B.empty)
            protect'
    return $ extractRev hs        
  where
    extractRev = B.tail . B.init . fromJust . lookup "Etag"


-- | Delete the given revision of the object.    
couchDelete :: MonadCouch m => 
       DocPath 
    -> Revision
    -> m ()
couchDelete p r = runResourceT $ couch HT.methodDelete p 
               [] [("rev", Just r)]
               (H.RequestBodyBS B.empty)
               protect' >> return () 

