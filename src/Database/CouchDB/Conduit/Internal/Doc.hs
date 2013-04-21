{-# LANGUAGE OverloadedStrings #-}

-- | Internal doc.
module Database.CouchDB.Conduit.Internal.Doc (
    couchRev,
    couchRev',
    couchDelete,
    
    couchGetWith,
    couchPutWith,
    couchPutWith_,
    couchPutWith'
) where

import              Prelude

import Control.Monad (void)
import Control.Exception.Lifted (catch, throw)

import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as A
import Data.Conduit (($$+-))
import qualified Data.Conduit.Attoparsec as CA

import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types as HT

import              Database.CouchDB.Conduit.Internal.Connection
import              Database.CouchDB.Conduit.LowLevel (couch, protect')
import              Database.CouchDB.Conduit.Internal.Parser

------------------------------------------------------------------------------
-- Type-independent methods
------------------------------------------------------------------------------

-- | Get Revision of a document. 
couchRev :: MonadCouch m => 
       Path                 -- ^ Correct 'Path' with escaped fragments.
    -> m Revision
couchRev p = do
    (H.Response _ _ hs _) <- couch HT.methodHead p [] [] 
                                 (H.RequestBodyBS B.empty) protect' 
    return $ peekRev hs        
  where
    peekRev = B.tail . B.init . fromJust . lookup "Etag"

-- | Brain-free version of 'couchRev'. If document absent, 
--   just return 'B.empty'.
couchRev' :: MonadCouch m =>
       Path                 -- ^ Correct 'Path' with escaped fragments.
    -> m Revision
couchRev' p = 
    catch (couchRev p) handler404
  where
    handler404 (CouchHttpError 404 _) = return B.empty
    handler404 e = throw e

-- | Delete the given revision of the object.    
couchDelete :: MonadCouch m => 
       Path                 -- ^ Correct 'Path' with escaped fragments.
    -> Revision             -- ^ Revision
    -> m ()
couchDelete p r = void $
  couch methodDelete p 
        [] [("rev", Just r)]
        (H.RequestBodyBS B.empty) protect'
               
------------------------------------------------------------------------------
-- with converter
------------------------------------------------------------------------------

-- | Load CouchDB document and parse it with given parser.  
couchGetWith :: MonadCouch m =>
          (A.Value -> A.Result a)    -- ^ Parser
       -> Path                       -- ^ Correct 'Path' with escaped fragments.
       -> Query                      -- ^ Query
       -> m (Revision, a)
couchGetWith f p q = do
    H.Response _ _ _ bsrc <- couch HT.methodGet 
                                 p [] q 
                                 (H.RequestBodyBS B.empty) protect'
    j <- bsrc $$+- CA.sinkParser A.json
    A.String r <- either throw return $ extractField "_rev" j
    o <- jsonToTypeWith f j 
    return (TE.encodeUtf8 r, o)

-- | Put document, with given encoder
couchPutWith :: MonadCouch m =>
      (a -> BL.ByteString)  -- ^ Encoder
   -> Path                  -- ^ Correct 'Path' with escaped fragments.
   -> Revision              -- ^ Document revision. For new docs provide 
                            -- ^ empty string.
   -> Query                 -- ^ Query arguments.
   -> a                     -- ^ The object to store.
   -> m Revision
couchPutWith f p r q val = do
    H.Response _ _ _ bsrc <- couch HT.methodPut 
                                 p (ifMatch r) q 
                                 (H.RequestBodyLBS $ f val) protect'
    j <- bsrc $$+- CA.sinkParser A.json
    either throw return $ extractRev j
  where 
    ifMatch "" = []
    ifMatch rv = [("If-Match", rv)]
    
-- | \"Don't care\"  version of version of 'couchPutWith'. Stores 
--   document only if it not exists.
couchPutWith_ :: MonadCouch m => 
      (a -> BL.ByteString)  -- ^ Encoder
   -> Path                  -- ^ Correct 'Path' with escaped fragments.
   -> HT.Query              -- ^ Query arguments.
   -> a                     -- ^ The object to store.
   -> m Revision      
couchPutWith_ f p q val = do
    rev <- couchRev' p
    if rev == "" 
        then couchPutWith f p "" q val
        else return rev

-- | Brute force version of 'couchPutWith'.
couchPutWith' :: MonadCouch m => 
      (a -> BL.ByteString)  -- ^ Encoder
   -> Path                  -- ^ Correct 'Path' with escaped fragments.
   -> HT.Query              -- ^ Query arguments.
   -> a                     -- ^ The object to store.
   -> m Revision      
couchPutWith' f p q val = do
    rev <- couchRev' p
    couchPutWith f p rev q val
