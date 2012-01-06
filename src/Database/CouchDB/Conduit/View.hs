{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.View (
    couchView
) where

import Control.Monad.Trans.Class (lift)
import Control.Applicative ((<|>))

import qualified Data.ByteString as B
import qualified Data.Aeson as A
import Data.Attoparsec
import Data.Attoparsec.Combinator

import Data.Conduit
import qualified Data.Conduit.Attoparsec as CA

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit

-- | Run CouchDB view.
couchView :: MonadCouch m =>
       DocPath              -- ^ Design document
    -> DocPath              -- ^ View name
    -> HT.Query             -- ^ Query
    -> Sink A.Object m a    -- ^ Sink
    -> m a
couchView designDocName viewName q sink = 
    runResourceT $ couch HT.methodGet fullPath [] q 
        (\_ _ bsrc -> bsrc $= conduitCouchView $$ sink)
        (H.RequestBodyBS B.empty)
  where
    fullPath = B.concat ["_design/", designDocName, "/_view/", viewName]

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