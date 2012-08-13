{-# LANGUAGE OverloadedStrings #-}

-- | CouchDB document attachments.
--
--   /Note about attachment paths:/ Attachments may have embedded @\/@ 
--   characters that are sent unescaped to CouchDB. You can use this to 
--   provide a subtree of attachments under a document. A DocID must have 
--   any @\/@ escaped as @%2F@. So if you have document @a\/b\/c@ with an 
--   attachment @d\/e\/f.txt@, you would be able to access it at 
--   @http:\/\/couchdb\/db\/a%2fb%2fc\/d\/e\/f.txt@. 
--
--   @couchdb-conduit@ automaticaly normalizes attachment paths.

module Database.CouchDB.Conduit.Attachment (
    couchGetAttach,
    couchPutAttach,
    couchDeleteAttach
) where

import Data.ByteString (ByteString)
import Data.Conduit (ResumableSource)

import Network.HTTP.Conduit (RequestBody(..))

import Database.CouchDB.Conduit.Internal.Connection 
        (MonadCouch (..), Path, Revision)

-- | Get document attachment and @Content-Type@.
couchGetAttach :: MonadCouch m =>
       Path             -- ^ Database
    -> Path             -- ^ Document
    -> ByteString       -- ^ Attachment path
    -> m (ResumableSource m ByteString, ByteString)
couchGetAttach = undefined

-- | Put or update document attachment
couchPutAttach :: MonadCouch m =>
       Path             -- ^ Database
    -> Path             -- ^ Document
    -> ByteString       -- ^ Attachment path
    -> Revision         -- ^ Document revision
    -> ByteString       -- ^ Attacment @Content-Type@
    -> RequestBody m    -- ^ Attachment body
    -> m Revision
couchPutAttach = undefined

-- | Delete document attachment
couchDeleteAttach :: MonadCouch m =>
       Path             -- ^ Database
    -> Path             -- ^ Document
    -> ByteString       -- ^ Attachment path
    -> Revision         -- ^ Document revision
    -> m Revision
couchDeleteAttach = undefined
