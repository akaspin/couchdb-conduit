{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-} 

module Database.CouchDB.Conduit.View 
(
    -- * Running views
    couchView,
    rowValue,
    -- * Manipulating views
    couchViewPut
)
 where

import              Prelude hiding (catch)

import              Control.Exception.Lifted (catch)

import              Control.Monad.Trans.Class (lift)
--import              Control.Monad.IO.Class (liftIO)
import              Control.Applicative ((<|>))

import              Data.Default
import              Data.Generics (Data, Typeable)
import qualified    Data.Map as Map
import qualified    Data.ByteString as B
import qualified    Data.HashMap.Lazy as M
import qualified    Data.Aeson as A
import              Data.Attoparsec

import              Data.Conduit
import qualified    Data.Conduit.List as CL
import qualified    Data.Conduit.Attoparsec as CA

import qualified    Network.HTTP.Conduit as H
import qualified    Network.HTTP.Types as HT

import              Database.CouchDB.Conduit
import qualified    Database.CouchDB.Conduit.Generic as CCG

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

-----------------------------------------------------------------------------
-- Manipulators
-----------------------------------------------------------------------------

-- | View. Just for internal.
data CouchDesignDoc = CouchDesignDoc {
      language :: B.ByteString
    , views :: Map.Map B.ByteString (Map.Map B.ByteString B.ByteString)
    } deriving (Show, Eq, Data, Typeable)
    
instance Default CouchDesignDoc where
    def = CouchDesignDoc { language = "javascript" ,
                           views = Map.empty }
    
-- | Helper for put new views in design documents.  
couchViewPut :: MonadCouch m =>
       DocPath              -- ^ Design document
    -> DocPath              -- ^ View name
    -> B.ByteString         -- ^ Language. \"javascript\" for example.
    -> B.ByteString         -- ^ 
    -> Maybe B.ByteString
    -> m Revision
couchViewPut designDocName viewName lang mapFn reduceFn = do
    (rev, dd@(CouchDesignDoc l vs)) <- catch
        (CCG.couchGet fullPath [])
        (\(_ :: CouchError) -> return (B.empty, def))
    if lang /= l then resourceThrow $ CouchError Nothing "Wrong language"
        else CCG.couchPut fullPath rev [] $
                dd { views = Map.insert viewName constructView vs}
  where
    fullPath = B.concat ["_design/", designDocName]
    constructView = let s = Map.singleton "map" mapFn in
        maybe s (\f -> Map.insert "reduce" f s) reduceFn                          
            




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