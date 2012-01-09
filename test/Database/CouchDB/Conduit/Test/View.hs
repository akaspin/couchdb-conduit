{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE ScopedTypeVariables #-}

module Database.CouchDB.Conduit.Test.View where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

import              Data.Generics (Data, Typeable)
import Data.Conduit
import qualified Data.Conduit.List as CL

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.View

tests :: Test
tests = mutuallyExclusive $ testGroup "View" [
    testCase "Basic" case_basicViewM
--    testCase "Basic" case_basicView,
--    testCase "Basic with reduce" case_basicViewReduce
    ]

data T = T {
    kind :: String,
    intV :: Int,
    strV :: String
    } deriving (Show, Eq, Data, Typeable)

case_manip :: Assertion
case_manip = runCouch conn $ do
    r' <- couchViewPut "test" "group3" "javascript" 
            "function(doc) {emit(null, doc);}" Nothing
    r'' <- couchViewPut "test1" "group3" "javascript" 
            "function(doc) {emit(null, doc);}" Nothing
    liftIO $ print (r', r'')

case_basicViewM :: Assertion
case_basicViewM = runCouch conn $ runResourceT $ do
    s <- couchView "test" "group1" [("reduce", Just "false")] 
    _ <- s $= (CL.mapM (liftIO . print)) $$ CL.consume
    s' <- couchView "test" "group1" [("reduce", Just "false")]
    _ <- s' $= rowValue $$ CL.mapM_ (liftIO . print)
    res' <- lift $ couchView' "test" "group1" [("reduce", Just "false")] $ 
        rowValue =$ CL.consume
    liftIO $ print res'

case_basicView :: Assertion
case_basicView = runCouch conn $  do
    _ <- couchView' "test" "group1" [("reduce", Just "false")] $ 
        (CL.mapM (liftIO . print)) =$ CL.consume
    _ <- couchView' "test" "group1" [("reduce", Just "false")] $ 
        rowValue =$ CL.mapM_ (liftIO . print)
    res' <- couchView' "test" "group1" [("reduce", Just "false")] $ 
        rowValue =$ CL.consume
    liftIO $ print res'

case_basicViewReduce :: Assertion
case_basicViewReduce = runCouch conn $ do
    res <- couchView' "test" "group1" [] $ 
        CL.mapM (liftIO . print) =$ CL.consume
    liftIO $ print res
    
conn :: CouchConnection
conn = def {couchDB = "cdbc_test"} 
