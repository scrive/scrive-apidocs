module Doc.Model.Search
  ( GetDocumentIdsWithNullSearchField(..)
  , updateHistoricalSearchData
  ) where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Reader
import Data.Time.Clock (diffUTCTime)
import Log

import DB
import Doc.DocumentID
import Doc.DocumentMonad (theDocumentID, withDocumentID)
import Doc.Model.Query ()
import KontraPrelude

updateHistoricalSearchData :: ( MonadDB m
                              , MonadThrow m
                              , MonadCatch m
                              , MonadTime m
                              , MonadLog m)
                           => m ()
updateHistoricalSearchData = do
  docIDs <- dbQuery $ GetDocumentIdsWithNullSearchField 1000
  t0 <- currentTime
  ress <- forM docIDs $ flip withDocumentID $ do
             docID <- theDocumentID
             dbUpdate $ SetDocumentSearchField docID
  unless (null docIDs) commit
  t1 <- currentTime
  logInfo "Document search data updated" $ object
          [ "items_updated" .= (length . filter id $ ress)
          , "items_failed" .= (length . filter not $ ress)
          , "elapsed_time" .= (realToFrac (diffUTCTime t1 t0) :: Double) ]


data GetDocumentIdsWithNullSearchField = GetDocumentIdsWithNullSearchField Int
instance MonadDB m => DBQuery m GetDocumentIdsWithNullSearchField [DocumentID] where
  query (GetDocumentIdsWithNullSearchField limit) = do
    runQuery_ . sqlSelect "documents" $ do
      sqlResult "id"
      sqlWhereIsNULL "archive_search_terms"
      sqlLimit limit
    fetchMany runIdentity

data SetDocumentSearchField = SetDocumentSearchField DocumentID
instance (MonadDB m, MonadThrow m) => DBUpdate m SetDocumentSearchField Bool where
  update (SetDocumentSearchField docID) = do
    -- We do a raw query here, since we rely on a stored procedure in the DB:
    -- `archive_search_terms_func(bigint)`, defined by means of
    -- `Doc.Trigger.archiveSearchTerms`.
    runQuery01 $ rawSQL
                   (
                     "WITH new_archive_search_terms(txt) AS" <+>
                     "( SELECT coalesce(archive_search_terms_func($1)) )" <+>
                     "UPDATE documents" <+>
                     "SET    archive_search_terms = (SELECT txt FROM new_archive_search_terms)," <+>
                     "       archive_search_fts = post_process_search_string((SELECT txt FROM new_archive_search_terms))" <+>
                     "WHERE  id = $1"
                   )
                   (Identity docID)
