module Doc.DBActions
    (
      GetAuthorUserID(..)
    , GetDocumentSearchDataByFunction(..)
    , GetDocumentSearchDataByField(..)
    , SetSLFValueTextField(..)
    ) where

import Control.Monad.Catch (MonadThrow)

import DB
import Doc.DocumentID
import Doc.Model.Query ()
import Doc.SignatoryFieldID
import User.Model

-- | Get the result of concatenating the different strings that we expose for
-- searching purposes. For testing purposes only.
newtype GetDocumentSearchDataByFunction = GetDocumentSearchDataByFunction DocumentID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentSearchDataByFunction (Maybe String) where
  query (GetDocumentSearchDataByFunction docID) = do
    runQuery_
      $ rawSQL "SELECT coalesce(archive_search_terms_func($1), '')" (Identity docID)
    fetchMaybe runIdentity

-- | Get the contents of the field `documents.archive_search_terms`. For testing
-- purposes only.
newtype GetDocumentSearchDataByField = GetDocumentSearchDataByField DocumentID
instance (MonadDB m, MonadThrow m) => DBQuery m GetDocumentSearchDataByField (Maybe String) where
  query (GetDocumentSearchDataByField docID) = do
    runQuery_ . sqlSelect "documents" $ do
      sqlResult "archive_search_terms"
      sqlWhereEq "id" docID
    join <$> fetchMaybe runIdentity

-- | Set the contents of the field `signatory_link_fields.value_text`. For
-- testing purposes only.
data SetSLFValueTextField = SetSLFValueTextField SignatoryFieldID String
instance (MonadDB m, MonadThrow m) => DBUpdate m SetSLFValueTextField () where
  update (SetSLFValueTextField sfid valueText) = do
    runQuery_ . sqlUpdate "signatory_link_fields" $ do
      sqlWhereEq "id" sfid
      sqlSet "value_text" valueText

newtype GetAuthorUserID = GetAuthorUserID DocumentID
instance (MonadDB m, MonadThrow m) => DBQuery m GetAuthorUserID (Maybe UserID) where
  query (GetAuthorUserID docID) = do
    runQuery_ . sqlSelect "documents" $ do
      sqlResult "author_user_id"
      sqlWhereEq "id" docID
    fetchMaybe runIdentity
