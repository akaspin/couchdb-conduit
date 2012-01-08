{-# LANGUAGE OverloadedStrings #-}

module Database.CouchDB.Conduit.Internal.Path (
    mkPath
) where

import qualified Data.Text.Encoding as TE (decodeUtf8, encodeUtf8)
import qualified Blaze.ByteString.Builder as BLB (toByteString)
import           Network.HTTP.Types (encodePathSegments)

import Database.CouchDB.Conduit (Path)

-- | Make correct path from escaped fragments.
mkPath :: [Path]    -- ^ Path fragments be escaped.  
       -> Path
mkPath = toByteString . encodePathSegments . map decodeUtf8 . filter (/="")
