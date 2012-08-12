{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Database.CouchDB.Conduit.Test.Attachment (tests) where

import           Test.Framework                      (Test, mutuallyExclusive,
                                                      testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, (@=?))

import           Control.Exception.Lifted            (bracket_)
import           Control.Monad ()
import           Control.Monad.IO.Class              (liftIO)

import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as BL

import           Data.Conduit
import qualified Data.Conduit.List                   as CL
import           Data.Generics                       (Data, Typeable)
import           Database.CouchDB.Conduit
import           Database.CouchDB.Conduit.Attachment
import           Database.CouchDB.Conduit.Generic
import           Database.CouchDB.Conduit.Test.Util
import Data.Text (Text)
import qualified Data.Text.Encoding                           as TE

tests :: Test
tests = mutuallyExclusive $ testGroup "Attachment"
        [ testCase "Just put-get-delete" caseJustPutGet
        , testCase "Just put-delete-get" caseJustPutDeleteGet
        , testCase "Just put-get-with-slashes" caseJustPutGetSlashes
        , testCase "multi-byte" caseMultiByte
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
      _ <- couchSimplePutAttach dbName "doc-just" "gandalf.html" rev
              testContentType testContent
      (src, ctype) <- couchGetAttach dbName "doc-just" "gandalf.html"
      content <- src $$+- CL.consume
      liftIO $ (BL.fromChunks content) @=? testContent
      liftIO $ ctype @=? testContentType


caseJustPutGetSlashes :: Assertion
caseJustPutGetSlashes = bracket_
    setup teardown $
    runCouch conn $ do
      let testContent1 = "Gandalf Gandalf Gandalf" ::B.ByteString
      let testContent2 = "Gandalf Bilbo Bilbo" ::B.ByteString
      let testContentType = "text/html"

      let contentSource1 = CL.sourceList [testContent1]
      let contentSource2 = CL.sourceList [testContent2]

      rev <- couchPut dbName "doc-just" "" [] $ TestDoc "doc" 1 "1"
      rev' <- couchPutAttach dbName "doc-just" "src/gandalf.html" rev
              testContentType (fromIntegral $ B.length testContent1) contentSource1
      rev'' <- couchPutAttach dbName "doc-just" "src%2fgandalf.html" rev'
               testContentType (fromIntegral $ B.length testContent2) contentSource2

      (src1, ctype1) <- couchGetAttach dbName "doc-just" "src/gandalf.html"
      (src2, ctype2) <- couchGetAttach dbName "doc-just" "src%2fgandalf.html"

      content1 <- src1 $$+- CL.consume
      content2 <- src2 $$+- CL.consume

      liftIO $ (B.concat content1) @=? testContent1
      liftIO $ ctype1 @=? testContentType
      liftIO $ (B.concat content2) @=? testContent2
      liftIO $ ctype2 @=? testContentType

caseJustPutDeleteGet :: Assertion
caseJustPutDeleteGet = bracket_
    setup teardown $
    runCouch conn $ do
      let testContent = "Frodo Frodo Frodo Frodo"
      let testContentType = "text/html"

      rev <- couchPut dbName "doc-just" "" [] $ TestDoc "doc" 1 "1"
      rev' <- couchSimplePutAttach dbName "doc-just" "frodo.html" rev
              testContentType testContent
      rev'' <- couchDelAttach dbName "doc-just" "frodo.html" rev'
      rev''' <- couchRev dbName "doc-just"
      liftIO $ rev'' @=? rev'''


caseMultiByte :: Assertion
caseMultiByte = bracket_
    setup teardown $
    runCouch conn $ do
      let testContent = "Гэндальф Гэндальф Гэндальф" ::Text
      let testContentType = "text/html"

      rev <- couchPut dbName "doc-just" "" [] $ TestDoc "doc" 1 "1"
      _ <- couchSimplePutAttach dbName "doc-just" "gandalf.html" rev
              (testContentType) (BL.fromChunks [TE.encodeUtf8 testContent])
      (src, ctype) <- couchGetAttach dbName "doc-just" "gandalf.html"
      content <- src $$+- CL.consume
      liftIO $ ( TE.decodeUtf8 $ B.concat content) @=? testContent
      liftIO $ ctype @=? testContentType


setup :: IO ()
setup = setupDB dbName
teardown :: IO ()
teardown = tearDB dbName

dbName :: B.ByteString
dbName = "cdbc_test_attachment"
