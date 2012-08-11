{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Database.CouchDB.Conduit.Test.Attachment (tests) where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))
import Database.CouchDB.Conduit.Test.Util

import Control.Exception.Lifted (bracket_)
import Control.Monad.IO.Class (liftIO)

import Data.ByteString (ByteString)

import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Attachment
import Database.CouchDB.Conduit.Generic

import Data.Generics (Data, Typeable)

tests :: Test
tests = mutuallyExclusive $ testGroup "Attachment"
        [ testCase "Just put-get-delete" caseJustPutGet
        , testCase "Just put-delete-get" caseJustPutDeleteGet
        , testCase "Just put-get-with-slashes" caseJustPutGetSlashes
        ]

data TestDoc = TestDoc { kind :: String, intV :: Int, strV :: String }
    deriving (Show, Eq, Data, Typeable)

caseJustPutGet :: Assertion
caseJustPutGet = bracket_
    setup teardown $
    runCouch conn $ do
      let testContent = "Gandalf Gandalf Gandalf"
      let testContentType = "text/html"

      rev <- couchPut dbName "doc-just" "" [] $ TestDoc "doc" 1 "1"
      _ <- couchPutAttach dbName "doc-just" "gandalf.html" rev
              testContentType testContent
      (content, ctype) <- couchGetAttach dbName "doc-just" "gandalf.html"
      liftIO $ content @=? testContent
      liftIO $ ctype @=? testContentType


caseJustPutGetSlashes :: Assertion
caseJustPutGetSlashes = bracket_
    setup teardown $
    runCouch conn $ do
      let testContent1 = "Gandalf Gandalf Gandalf"
      let testContent2 = "Gandalf Bilbo Bilbo"
      let testContentType = "text/html"

      rev <- couchPut dbName "doc-just" "" [] $ TestDoc "doc" 1 "1"
      rev' <- couchPutAttach dbName "doc-just" "src/gandalf.html" rev
              testContentType testContent1
      rev'' <- couchPutAttach dbName "doc-just" "src%2fgandalf.html" rev'
               testContentType testContent2

      (content1, ctype1) <- couchGetAttach dbName "doc-just" "src/gandalf.html"
      (content2, ctype2) <- couchGetAttach dbName "doc-just" "src%2fgandalf.html"

      liftIO $ content1 @=? testContent1
      liftIO $ ctype1 @=? testContentType
      liftIO $ content2 @=? testContent2
      liftIO $ ctype2 @=? testContentType

caseJustPutDeleteGet :: Assertion
caseJustPutDeleteGet = bracket_
    setup teardown $
    runCouch conn $ do
      let testContent = "Frodo Frodo Frodo Frodo"
      let testContentType = "text/html"

      rev <- couchPut dbName "doc-just" "" [] $ TestDoc "doc" 1 "1"
      rev' <- couchPutAttach dbName "doc-just" "frodo.html" rev
              testContentType testContent
      rev'' <- couchDelAttach dbName "doc-just" "frodo.html" rev'
      rev''' <- couchRev dbName "doc-just"
      liftIO $ rev'' @=? rev'''


setup :: IO ()
setup = setupDB dbName
teardown :: IO ()
teardown = tearDB dbName

dbName :: ByteString
dbName = "cdbc_test_attachment"