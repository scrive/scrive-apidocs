{-# LANGUAGE CPP #-}

module Doc.Transitory 
 ( module Doc.DocState
 , doc_update
 , doc_query
 ) where


#if 1
import Doc.DocState
import Happstack.State
import Control.Monad.IO.Class

doc_update :: (MonadIO m, UpdateEvent ev res) => ev -> m res
doc_update = update

doc_query :: (MonadIO m, QueryEvent ev res) => ev -> m res
doc_query = query
#endif




