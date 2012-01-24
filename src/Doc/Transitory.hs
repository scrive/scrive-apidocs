{-# LANGUAGE CPP #-}

module Doc.Transitory 
 (
module Doc.Model
 , doc_update
 , doc_update'
 , doc_query
 , doc_query'
 ) where


import Doc.Model
import DB.Classes

doc_update :: (DBUpdate q r, DBMonad m) => q -> m r
doc_update = runDB . dbUpdate

doc_update' :: (DBUpdate q r) => q -> DB r
doc_update' = dbUpdate

doc_query :: (DBQuery q r, DBMonad m) => q -> m r
doc_query = runDB . dbQuery

doc_query' :: (DBQuery q r) => q -> DB r
doc_query' = dbQuery




