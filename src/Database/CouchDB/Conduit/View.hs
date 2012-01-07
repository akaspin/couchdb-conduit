{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveDataTypeable #-} 

module Database.CouchDB.Conduit.View 
(
    -- * Running views
    couchView,
    rowValue,
    -- * Manipulating views
    CouchDesignDoc(..),
    CouchViewDoc(..)
)
 where

import              Control.Monad (unless)
import              Control.Monad.Trans.Class (lift)
import              Control.Monad.IO.Class (liftIO)
import              Control.Applicative ((<|>))

import              Data.Generics (Data, Typeable)
import qualified Data.Map as Map
import qualified    Data.ByteString as B
import qualified    Data.ByteString.UTF8 as BU8
import qualified    Data.Text.Encoding as TE
import qualified    Data.HashMap.Lazy as M
import qualified    Data.Aeson as A
import              Data.Aeson ((.=))
import              Data.Attoparsec

import              Data.Conduit
import qualified    Data.Conduit.List as CL
import qualified    Data.Conduit.Attoparsec as CA

import qualified    Network.HTTP.Conduit as H
import qualified    Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import              Database.CouchDB.Conduit.Internal.Doc
import              Database.CouchDB.Conduit.Internal.Parser

-- | Run CouchDB view.
couchView :: MonadCouch m =>
       DocPath              -- ^ Design document
    -> DocPath              -- ^ View name
    -> HT.Query             -- ^ Query
    -> Sink A.Object m a    -- ^ Sink for handle view rows
    -> m a
couchView designDocName viewName q sink = runResourceT $ do
    H.Response _ _ bsrc <-  couch HT.methodGet fullPath [] q 
        (H.RequestBodyBS B.empty) protect'
    bsrc $= conduitCouchView $$ sink
  where
    fullPath = B.concat ["_design/", designDocName, "/_view/", viewName]

-- | Conduit for extract \"value\" field.
rowValue :: ResourceIO m => Conduit A.Object m A.Value
rowValue = CL.mapM (\v -> case M.lookup "value" v of
                (Just o) -> return o
                _ -> resourceThrow $ CouchError Nothing $ 
                        "View row does not contain value: " ++ show v)

data CouchDesignDoc = CouchDesignDoc {
      language :: String
--    , views :: Map.Map String CouchViewDoc
    } deriving (Show, Eq, Data, Typeable)
    
data CouchViewDoc = CouchViewDoc {
    map :: String,
    reduce :: Maybe String
    } deriving (Show, Eq, Data, Typeable)
    
-- | Helper for put new views
couchViewPut :: MonadCouch m =>
       DocPath              -- ^ Design document
    -> DocPath              -- ^ View name
    -> B.ByteString
    -> B.ByteString
    -> Maybe B.ByteString
    -> m Revision
couchViewPut designDocName viewName lang mapFn reduceFn = do
    raw <- couchGetRaw fullPath []
    rev <- either resourceThrow return $ valToRev raw
    checkLang raw
    undefined
  where
    fullPath = B.concat ["_design/", designDocName, "/", viewName]
    checkLang (A.Object j) = 
        case M.lookup "language" j of
            Nothing -> resourceThrow $ CouchError Nothing "Language absent"
            (Just (A.String l)) -> unless
                    (l == TE.decodeUtf8 lang) $ 
                    resourceThrow $ CouchError Nothing $ 
                        "Languages not match. " ++ 
                        (BU8.toString . TE.encodeUtf8 $ l) ++
                        " : " ++ BU8.toString lang
            _ -> resourceThrow $ CouchError Nothing "Bad view" 
    checkLang _ = resourceThrow $ CouchError Nothing "Bad view"
    
    constructView = TE.decodeUtf8 viewName .= A.object (
            maybe ["map" .= mapFn] 
                  (\r -> ["map" .= mapFn, "reduce" .= r])
                  reduceFn )
            




-----------------------------------------------------------------------------
-- Internal Parser conduit
-----------------------------------------------------------------------------

conduitCouchView :: ResourceIO m => Conduit B.ByteString m A.Object
conduitCouchView = sequenceSink () $ \() -> do
    b <- CA.sinkParser viewStart
    if b then return $ StartConduit viewLoop
         else return Stop

viewLoop :: ResourceIO m => Conduit B.ByteString m A.Object   
viewLoop = sequenceSink False $ \isLast -> 
    if isLast then return Stop
    else do 
        v <- CA.sinkParser (A.json <?> "json object")
        vobj <- case v of
            (A.Object o) -> return o
            _ -> lift $ resourceThrow $ 
                 CouchError Nothing "view entry is not an object"
        res <- CA.sinkParser (commaOrClose <?> "comma or close")
        case res of
            Comma -> return $ Emit False [vobj]
            CloseBracket -> return $ Emit True [vobj]

data CommaOrCloseBracket = Comma | CloseBracket

commaOrClose :: Parser CommaOrCloseBracket
commaOrClose = do
    skipWhile (\c -> c /= 44 && c /= 93) <?> 
            "Checking for next comma"
    w <- anyWord8
    if w == 44 then return Comma else return CloseBracket

-- determine view
viewStart :: Parser Bool
viewStart = do
    _ <- string "{" 
    _ <- option "" $ string "\"total_rows\":" 
    option () $ skipWhile (\x -> x >= 48 && x <= 57)
    _ <- option "" $ string ",\"offset\":"
    option () $ skipWhile (\x -> x >= 48 && x <= 57)
    _ <- option "" $ string ","
    _ <- string "\"rows\":["
    (string "]}" >> return False) <|> return True