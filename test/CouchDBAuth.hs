{-# LANGUAGE OverloadedStrings #-} 


module CouchDBAuth (login, pass) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()  -- just orchan

login, pass :: ByteString
-- | Set your login
login = "root"
-- | Set your pass
pass = "sumatra"
