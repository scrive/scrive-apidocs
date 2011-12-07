{-# LANGUAGE CPP #-}

module Doc.Transitory 
 (
#ifndef DOCUMENTS_IN_POSTGRES
module Doc.DocState
#else
module Doc.Model
#endif

 , doc_update
 , doc_query
 ) where



#ifndef DOCUMENTS_IN_POSTGRES
import Doc.DocState
import Happstack.State
import Control.Monad.IO.Class

doc_update :: (MonadIO m, UpdateEvent ev res) => ev -> m res
doc_update = update

doc_query :: (MonadIO m, QueryEvent ev res) => ev -> m res
doc_query = query


#else


import Doc.Model
import DB.Classes

doc_update :: (DBUpdate q r, DBMonad m) => q -> m r
doc_update = runDB . dbUpdate

doc_query :: (DBQuery q r, DBMonad m) => q -> m r
doc_query = runDB . dbQuery
#endif




