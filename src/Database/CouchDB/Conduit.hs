{- | CouchDB 

To work with concrete objects, use the following modules:
  
  * "Database.CouchDB.Conduit.DB" Database
  
  * "Database.CouchDB.Conduit.View" Views
  
  * "Database.CouchDB.Conduit.Explicit" Explicit JSON methods
  
  * "Database.CouchDB.Conduit.Generic" Generic JSON methods
  
  * "Database.CouchDB.Conduit.LowLevel" Low-level methods
   
For complete documentation about The Couch DB HTTP API see
<http://wiki.apache.org/couchdb/Complete_HTTP_API_Reference>
-}

module Database.CouchDB.Conduit (
    -- * Document paths and revisions #path#
    Path,
    mkPath,
    Revision,
    
    -- * CouchDB Connection #connection#
    CouchConnection,
    def,
    couchHost,
    couchPort,
    couchManager,
    couchDB,
    couchLogin,
    couchPass,
    
    -- * Runtime enviroment and errors #runtime#
    MonadCouch (..),
    CouchError (..),
    runCouch,
    withCouchConnection
) where

import Database.CouchDB.Conduit.Internal.Connection

