{-# LANGUAGE OverloadedStrings #-} 

-- | Query tests

module Database.CouchDB.Conduit.Test.Query (tests) where

import Test.Framework (testGroup, mutuallyExclusive, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Control.Monad.IO.Class (liftIO)

import Database.CouchDB.Conduit.View.Query

tests :: Test
tests = mutuallyExclusive $ testGroup "Generic" [
    testCase "Descending" caseDescending
    ]

caseDescending :: Assertion    
caseDescending = liftIO $ do
    mkQuery [QPInt "i" 0] @=? [("i", Just "0")]
    mkQuery [QPDescending, QPInt "i" 0] @=? 
            [desc, ("i", Just "0")]
    let swappable = [QPStartKey (0::Int), QPEndKey (1::Int)]
    mkQuery swappable @=? 
            [("startkey", Just "0"), ("endkey", Just "1")]
    mkQuery (QPDescending : swappable) @=? 
            [desc, ("endkey", Just "0"), ("startkey", Just "1")]
  where
    desc = ("descending", Just "true")

