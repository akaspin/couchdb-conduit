{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-} 

-- | CouchDB View Query options.
--
--   For details see 
--   <http://wiki.apache.org/couchdb/HTTP_view_API#Querying_Options>. Note, 
--   because all options must be a proper URL encoded JSON, construction of 
--   complex parameters can be very tedious. To simplify this, use 'mkQuery'.
 
module Database.CouchDB.Conduit.View.Query (
    -- * Creating Query
    CouchQP(..),
    mkQuery,
    
    -- * Parameter helpers
    qpUnit,
    qpNull
) where

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as MS
import qualified Data.Aeson as A
import Data.String.Conversions (cs, (<>))

import qualified Network.HTTP.Types as HT

import Database.CouchDB.Conduit.Internal.Connection (Path)

-- | CouchDB Query options primitives.
data CouchQP =
      forall a . A.ToJSON a => QPComplex B.ByteString a
        -- ^ Complex view query parameter.  
        --
        -- > couchQP [QPComplex "param" (["a", "b"] :: [String])]
        -- > [("param", Just "[\"a\",\"b\"]")]
        -- > ...?param=["a","b"]
        -- > 
        -- > couchQP [QPComplex "key" (("a", 1) :: (String, Int))]
        -- > [("key", Just "[\"a\",0]")]
        -- > ...?param=["a",0]
        --
        -- It't just convert lazy 'BL.ByteString' from 'A.encode' to strict 
        -- 'B.ByteString'. For more efficient use specific functions. 
    
    | QPBS B.ByteString B.ByteString
        -- ^ Quoted 'B.ByteString' query parameter.
        --
        -- > ...?param="value" 
    
    | QPInt B.ByteString Int
        -- ^ 'Int' query parameter.
        --
        -- > ...?param=100 
    
    | QPBool B.ByteString Bool
        -- ^ 'Bool' query parameter.
        --
        -- > ...?param=true
    
    | QPLimit Int
        -- ^ Limit rows. Use @Zero (0)@ to omit.
        --
        -- > ...?limit=5 
    
    | QPSkip Int
        -- ^ Skip rows. Use @Zero (0)@ to omit.
        --
        -- > ...?skip=10
    
    | QPStale Bool
        -- ^ Stale view. On @True@ sets @stale@ parameter to @ok@, else 
        --   sets it to @update_after@.
        --    
        -- > ...?stale=ok
        -- > ...?stale=update_after
    
    | forall a . A.ToJSON a => QPKey a
        -- ^ @key=...@ query parameter.
        -- > key=...
        
    | forall a . A.ToJSON a => QPStartKey a
        -- ^ Row key to start with. Becomes @endkey@ if @descending@ turned on. 
        --   See 'couchQuery'. 
        --
        -- > ...?startkey=...
        -- > ...?descending=true?endkey=...
        
    | forall a . A.ToJSON a => QPEndKey a
        -- ^ Row key to start with. Becomes @startkey@ if @descending@ 
        --   turned on. See 'couchQuery'. 
        --
        -- > ...?endkey=...
        -- > ...?descending=true?startkey=...
        
    | forall a . A.ToJSON a => QPKeys a
        -- ^ Row key to start with. Use only with 'couchView' and 
        --   'couchView_'. For large sets of @keys@ use 'couchViewPost' and 
        --   'couchViewPost_'
        --
        -- > ...?keys=...
        
    | QPGroup
        -- ^ Turn on grouping.
        --
        -- > ...?group=true
    | QPGroupLevel Int
        -- ^ Set grouping level. Use @Zero (0)@ to omit.
        --
        -- > ...?group_level=2
    | QPReduce Bool
        -- ^ Control reduce.
        --
        -- > ...?reduce=true
        -- > ...?reduce=false
        
    | QPIncludeDocs
        -- ^ Turn on inclusion docs in view results.
        --
        -- > ...?include_docs=true
        
    | QPInclusiveEnd
        -- ^ Turn off inclusion @endkey@ in view results.
        --
        -- > ...?inclusive_end=false

    | QPUpdateSeq
        -- ^ Response includes an update_seq value indicating which sequence 
        --   id of the database the view reflects
        --
        -- > ...?update_seq=true
        
    | QPStartKeyDocId Path
        -- ^ Document id to start with.
        --
        -- > ...?startkey_docid=...
    | QPEndKeyDocId Path
        -- ^ Document id to end with.
        --
        -- > ...?endkey_docid=...
        
-- | Make CouchDB query options.
mkQuery :: 
       Bool         -- ^ Descending sort order. On @True@ adds 
                    --   @descending@ to query. Also swaps 
                    --   @start_key@ and @end_key@ parameters            
    -> [CouchQP]    -- ^ Query options.
    -> HT.Query
mkQuery desc = 
    parseDesc desc . concatMap parseqp
  where
    parseDesc True = (("descending", Just "true"):)
    parseDesc False = id
    parseqp (QPComplex n v) = [(n, Just $ cs . A.encode $ v)]  
    parseqp (QPBS n v) = [(n, Just $ "\"" <> v <> "\"")]  
    parseqp (QPInt n v) = [(n, Just $ cs . show $ v)]  
    parseqp (QPBool n True) = [(n, Just "true")]  
    parseqp (QPBool n False) = [(n, Just "false")]  
    parseqp (QPLimit v) = intZeroQp "limit" v  
    parseqp (QPSkip v) = intZeroQp "skip" v
    parseqp (QPStale True) = [("stale", Just "ok")]
    parseqp (QPStale False) = [("stale", Just "update_after")]
    parseqp (QPKey v) = parseqp $ QPComplex "key" v
    parseqp (QPKeys v) = parseqp $ QPComplex "keys" v
    parseqp (QPStartKey v) = parseqp $ QPComplex 
            (descDep "startkey" "endkey") v
    parseqp (QPEndKey v) = parseqp $ QPComplex 
            (descDep "endkey" "startkey") v
    parseqp QPGroup = boolqp "group" True  
    parseqp (QPGroupLevel v) = intZeroQp "group_level" v  
    parseqp (QPReduce v) = boolqp "reduce" v
    parseqp QPIncludeDocs = boolqp "include_docs" True
    parseqp QPInclusiveEnd = boolqp "inclusive_end" False
    parseqp QPUpdateSeq = boolqp "update_seq" True
    parseqp (QPStartKeyDocId v) = parseqp $ QPComplex "startkey_docid" v
    parseqp (QPEndKeyDocId v) = parseqp $ QPComplex "endkey_docid" v
    
    -- | Boolean
    boolqp n v = parseqp $ QPBool n v
    -- | Ommitable int
    intZeroQp _ 0 = []
    intZeroQp n v = parseqp $ QPInt n v
    -- | Descending dependent param
    descDep a b = if desc then b else a



-- | Returns empty 'MS.HashMap'. Aeson will convert 
--   this to @\{\}@ (JSON unit). This useful for @startkey@ and @endkey@.
--   
-- > couchQuery [QPStartKey (1, 0), QPEndKey (1, {})]
qpUnit :: MS.HashMap B.ByteString Bool
qpUnit = MS.empty

-- | Simply return 'A.Null'.
qpNull :: A.Value
qpNull = A.Null
