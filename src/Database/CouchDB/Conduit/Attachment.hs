{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.Conduit.Attachment
       ( couchPutAttach
       , couchGetAttach
       , couchDelAttach) where

import Data.Maybe
import Database.CouchDB.Conduit.Internal.Connection
            (MonadCouch(..), Path, mkPath, Revision)
import Database.CouchDB.Conduit.LowLevel (couch, protect')

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Data.Text.Encoding as TE

import qualified Data.Aeson as A
import Data.Conduit (($$+-))
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import Control.Exception.Lifted (throw)
import Database.CouchDB.Conduit.Internal.Parser

-- | Upload attachment.
couchPutAttach :: MonadCouch m =>
                  Path             -- ^ Database
                  -> Path           -- ^ Document
                  -> Path           -- ^ Attachment
                  -> Revision       -- ^ Document revision
                  -> B.ByteString   -- ^ Attachent content type
                  -> BL.ByteString  -- ^ Attachment content
                  -> m Revision
couchPutAttach db doc att rev contentType content =
  do H.Response _ _ _ bsrc <- couch HT.methodPut
                              (mkPath [db,doc,att])
                              [(HT.hContentType, contentType)]
                              (if rev /= "" then [("rev", Just rev)] else [])
                              (H.RequestBodyLBS content)
                              protect'
     j <- bsrc $$+- CA.sinkParser A.json
     A.String r <- either throw return $ extractField "rev" j
     return $ TE.encodeUtf8 r


-- | Get attachment.
couchGetAttach :: MonadCouch m =>
                  Path             -- ^ Database
                  -> Path           -- ^ Document
                  -> Path           -- ^ Attachment
                  -> m (BL.ByteString, B.ByteString) -- ^ (Content, Content-type)
couchGetAttach db doc att =
  do H.Response _ _ hs bsrc <- couch HT.methodGet
                               (mkPath [db,doc,att])
                               []
                               []
                               (H.RequestBodyBS B.empty)
                               protect'
     body <- bsrc $$+- CL.consume
     let contentType = peekContentType hs
     return (BL.fromChunks body, contentType)
  where
    peekVal a b = fromJust $ lookup a b
    peekContentType a = peekVal "Content-Type" a


-- | Delete attachment.
couchDelAttach :: MonadCouch m =>
                  Path             -- ^ Database
                  -> Path           -- ^ Document
                  -> Path           -- ^ Attachment
                  -> Revision       -- ^ Document revision
                  -> m (Revision)
couchDelAttach db doc att rev =
    do H.Response _ _ _ bsrc <- couch HT.methodDelete
                                (mkPath [db,doc,att])
                                []
                                [("rev", Just rev)]
                                (H.RequestBodyBS B.empty)
                                protect'
       j <- bsrc $$+- CA.sinkParser A.json
       A.String r <- either throw return $ extractField "rev" j
       return $ TE.encodeUtf8 r
