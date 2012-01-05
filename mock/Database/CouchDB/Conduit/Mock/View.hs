{-# LANGUAGE OverloadedStrings #-} 

module Database.CouchDB.Conduit.Mock.View (tests) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import Control.Monad.Base (liftBase)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Applicative ((<|>))

import Data.Aeson
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)
import Data.Attoparsec

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Attoparsec as CA

import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Db
import Database.CouchDB.Conduit.Explicit

tests :: Test
tests = testGroup "ViewMock" [
    testCase "Extract" mock_extract
--    testCase "boot" case_bootStrap
    ]

mock_extract :: Assertion
mock_extract = runCouch "localhost" 9500 "cdbc_test" $ do
    bsrc <- runResourceT $ couch HT.methodGet
        "_design/test/_view/group1"
        [] 
        [] 
        (\_ _ s -> return s)
        (H.RequestBodyBS B.empty)
    runResourceT $ bsrc $= conduitCouchView $$ CL.mapM_ (\a -> lift $ print a)
    return ()
--    liftBase $ print $ res

conduitCouchView :: ResourceIO m => Conduit B.ByteString m Object
conduitCouchView = sequenceSink () $ \() -> do
    b <- CA.sinkParser viewStart
    if b then return $ StartConduit viewLoop
         else return Stop

viewLoop :: ResourceIO m => Conduit B.ByteString m Object   
viewLoop = sequenceSink [] $ \state -> do
    v <- CA.sinkParser (json <?> "json object")
--    liftIO $ print v
    vobj <- case v of
        (Object o) -> return o
        _ -> lift $ resourceThrow $ 
             CouchError Nothing "view entry is not an object"
    res <- CA.sinkParser (commaOrClose <?> "comma or close")
    case res of
        Comma -> return $ Emit state [vobj]
        CloseBracket -> return Stop

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
    _ <- string "{\"total_rows\":"
    skipWhile (\x -> x >= 48 && x <= 57)
    _ <- string ",\"offset\":"
    skipWhile (\x -> x >= 48 && x <= 57)
    _ <- string ",\"rows\":["
    (string "]}" >> return False) <|> return True

passThrough :: Monad m => Conduit input m input
passThrough = Conduit $ return PreparedConduit
    { conduitPush = \input -> return $ Producing [input]
    , conduitClose = return []
    }

-- Boot
case_bootStrap :: Assertion
case_bootStrap =  runCouch "localhost" 9500 "cdbc_test" $ do
    couchDeleteDb ""
    couchPutDb ""
    mapM_ prepDoc ([1..5] :: [Int]) 
  where
    prepDoc n = do
        _ <- couchPut (fromString $ "doc-" ++ show n) "" [] $ object [ 
             "kind" .= ("doc" :: String),
             "intV" .= (n :: Int), 
             "strV" .= (show n :: String) ]
        mapM_ (prepNest n) ([1..5] :: [Int])
        
    prepNest p n' = couchPut (fromString $ "nest-" ++ show p ++ "-" ++ show n') 
            "" [] $ object [ 
             "kind" .= ("nest" :: String),
             "parent" .= (p :: Int),
             "nestIntV" .= (n' :: Int), 
             "nestStrV" .= (show n' :: String) ]

