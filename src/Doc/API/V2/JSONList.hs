{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V2.JSONList (DocumentFiltering, toDocumentSorting, toDocumentFilter) where

import Control.Applicative.Free
import Doc.DocStateData
import KontraPrelude
import Data.Unjson
import Doc.API.V2.UnjsonUtils
import Doc.API.V2.JSONMisc()
import User.UserID
import MinutesTime
import Data.Text
import Doc.Model.OrderBy
import DB
import qualified Doc.Model.Filter as DF

data DocumentSort = DocumentSort DocumentSortOn DocumentSortOrder

data DocumentSortOn = DocumentSortStatus | DocumentSortTitle | DocumentSortTime| DocumentSortAuthor deriving Eq
data DocumentSortOrder = DocumentSortAsc | DocumentSortDesc  deriving Eq

instance Unjson DocumentSortOrder where
  unjsonDef = unjsonEnumBy "DocumentSortOn" [
      (DocumentSortAsc, "ascending")
    , (DocumentSortDesc, "descending")
    ]

instance Unjson DocumentSortOn where
  unjsonDef = unjsonEnumBy "DocumentSortOn" [
      (DocumentSortStatus, "status")
    , (DocumentSortTitle, "title")
    , (DocumentSortTime, "mtime")
    , (DocumentSortAuthor, "author")
    ]

instance Unjson DocumentSort where
  unjsonDef = objectOf $ pure DocumentSort
    <*> field "sort_by" (\(DocumentSort v _) -> v) "How documents should be sorted"
    <*> fieldDef "order" DocumentSortAsc (\(DocumentSort _ o) -> o) "Descending or ascending sorting"


toDocumentSorting ::  DocumentSort -> AscDesc DocumentOrderBy
toDocumentSorting (DocumentSort DocumentSortStatus DocumentSortAsc) = Asc DocumentOrderByStatus
toDocumentSorting (DocumentSort DocumentSortTitle DocumentSortAsc) = Asc DocumentOrderByTitle
toDocumentSorting (DocumentSort DocumentSortTime DocumentSortAsc) = Asc DocumentOrderByMTime
toDocumentSorting (DocumentSort DocumentSortAuthor DocumentSortAsc) = Asc DocumentOrderByAuthor
toDocumentSorting (DocumentSort DocumentSortStatus DocumentSortDesc) = Desc DocumentOrderByStatus
toDocumentSorting (DocumentSort DocumentSortTitle DocumentSortDesc) = Desc DocumentOrderByTitle
toDocumentSorting (DocumentSort DocumentSortTime DocumentSortDesc) = Desc DocumentOrderByMTime
toDocumentSorting (DocumentSort DocumentSortAuthor DocumentSortDesc) = Desc DocumentOrderByAuthor


type DocumentFiltering = [DocumentFilter]

data DocumentFilter = DocumentFilterStatuses [DocumentStatus]
                    | DocumentFilterTime (Maybe UTCTime) (Maybe UTCTime)
                    | DocumentFilterTag Text Text
                    | DocumentFilterIsAuthor
                    | DocumentFilterIsAuthoredBy UserID
                    | DocumentFilterIsSignableOnPad
                    | DocumentFilterIsTemplate Bool
                    | DocumentFilterIsInTrash Bool
                    | DocumentFilterByText Text
                    | DocumentFilterCanBeSignedBy UserID


filterType ::  DocumentFilter -> Text
filterType (DocumentFilterStatuses _) = "status"
filterType (DocumentFilterTime _ _) = "mtime"
filterType (DocumentFilterTag _ _) = "tag"
filterType (DocumentFilterIsAuthor) = "is_author"
filterType (DocumentFilterIsAuthoredBy _) = "author"
filterType (DocumentFilterIsSignableOnPad) = "is_signable_on_pad"
filterType (DocumentFilterIsTemplate _) = "template"
filterType (DocumentFilterIsInTrash _) = "trash"
filterType (DocumentFilterByText _) = "text"
filterType (DocumentFilterCanBeSignedBy _) = "user_can_sign"

instance Unjson DocumentFilter where
  unjsonDef = disjointUnionOf "filter_by" $ filterMatch <$> [
        (DocumentFilterStatuses [], unjsonDocumentFilterStatuses)
      , (DocumentFilterTime Nothing Nothing, unjsonDocumentFilterTime)
      , (DocumentFilterTag "" "", unjsonDocumentFilterTag)
      , (DocumentFilterIsAuthor, unjsonDocumentFilterIsAuthor)
      , (DocumentFilterIsAuthoredBy (unsafeUserID 0), unjsonDocumentFilterIsAuthoredBy)
      , (DocumentFilterIsSignableOnPad, unjsonDocumentFilterIsSignableOnPad)
      , (DocumentFilterIsTemplate False, unjsonDocumentFilterIsTemplate)
      , (DocumentFilterIsInTrash False, unjsonDocumentFilterIsInTrash)
      , (DocumentFilterByText "", unjsonDocumentFilterByText)
      , (DocumentFilterCanBeSignedBy (unsafeUserID 0), unjsonDocumentFilterCanBeSignedBy)
    ]
    where
      filterMatch :: (DocumentFilter,Ap (FieldDef DocumentFilter) DocumentFilter) -> (Text, DocumentFilter -> Bool, Ap (FieldDef DocumentFilter) DocumentFilter)
      filterMatch (df,a) = (filterType df, \f -> filterType df == filterType f, a)

unjsonDocumentFilterStatuses:: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterStatuses = pure DocumentFilterStatuses
  <*  fieldReadonly "filter_by" filterType "Type of filter"
  <*> field "statuses" unsafeDocumentFilterStatuses"Statuses to filter on"
  where
    unsafeDocumentFilterStatuses:: DocumentFilter ->  [DocumentStatus]
    unsafeDocumentFilterStatuses(DocumentFilterStatuses fs) = fs
    unsafeDocumentFilterStatuses _ = $unexpectedError "unsafeDocumentFilterStatus"

unjsonDocumentFilterTime :: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterTime = pure DocumentFilterTime
  <*  fieldReadonly "filter_by" filterType "Type of filter"
  <*> fieldOpt "start_time" unsafeDocumentFilterStartTime "Only documents after start time"
  <*> fieldOpt "end_time" unsafeDocumentFilterEndTime "Only documents before end time"
  where
    unsafeDocumentFilterStartTime :: DocumentFilter ->  Maybe UTCTime
    unsafeDocumentFilterStartTime (DocumentFilterTime s _) = s
    unsafeDocumentFilterStartTime _ = $unexpectedError "unsafeDocumentFilterStartTime"
    unsafeDocumentFilterEndTime :: DocumentFilter ->  Maybe UTCTime
    unsafeDocumentFilterEndTime (DocumentFilterTime _ e) = e
    unsafeDocumentFilterEndTime _ = $unexpectedError "unsafeDocumentFilterEndTime"

unjsonDocumentFilterTag:: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterTag = pure DocumentFilterTag
  <*  fieldReadonly "filter_by" filterType "Type of filter"
  <*> field "name" unsafeDocumentFilterTagName "Name of tag to filter on"
  <*> field "value" unsafeDocumentFilterTagValue "Value of such tag"
  where
    unsafeDocumentFilterTagName:: DocumentFilter ->  Text
    unsafeDocumentFilterTagName (DocumentFilterTag n _) = n
    unsafeDocumentFilterTagName _ = $unexpectedError "unsafeDocumentFilterTagName"
    unsafeDocumentFilterTagValue:: DocumentFilter ->  Text
    unsafeDocumentFilterTagValue (DocumentFilterTag _ v) = v
    unsafeDocumentFilterTagValue _ = $unexpectedError "unsafeDocumentFilterTagValue"

unjsonDocumentFilterIsAuthor:: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterIsAuthor = pure DocumentFilterIsAuthor
  <*  fieldReadonly "filter_by" filterType "Type of filter"

unjsonDocumentFilterIsAuthoredBy:: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterIsAuthoredBy = pure DocumentFilterIsAuthoredBy
  <*  fieldReadonly "filter_by" filterType "Type of filter"
  <*> field "user_id" unsafeDocumentFilterUserID "Id of author"
  where
    unsafeDocumentFilterUserID :: DocumentFilter ->  UserID
    unsafeDocumentFilterUserID (DocumentFilterIsAuthoredBy uid) = uid
    unsafeDocumentFilterUserID _ = $unexpectedError "unsafeDocumentFilterStatus"

unjsonDocumentFilterIsSignableOnPad :: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterIsSignableOnPad = pure DocumentFilterIsSignableOnPad
  <*  fieldReadonly "filter_by" filterType "Type of filter"


unjsonDocumentFilterIsTemplate:: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterIsTemplate = pure DocumentFilterIsTemplate
  <*  fieldReadonly "filter_by" filterType "Type of filter"
  <*> field "is_template" unsafeDocumentFilterIsTemplate "Filter documents that are templates"
  where
    unsafeDocumentFilterIsTemplate :: DocumentFilter ->  Bool
    unsafeDocumentFilterIsTemplate (DocumentFilterIsTemplate is_template) = is_template
    unsafeDocumentFilterIsTemplate _ = $unexpectedError "unsafeDocumentFilterIsTemplate"

unjsonDocumentFilterIsInTrash:: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterIsInTrash = pure DocumentFilterIsInTrash
  <*  fieldReadonly "filter_by" filterType "Type of filter"
  <*> field "is_trashed" unsafeDocumentFilterIsTrashed "Filter documents that are in trash"
  where
    unsafeDocumentFilterIsTrashed :: DocumentFilter ->  Bool
    unsafeDocumentFilterIsTrashed (DocumentFilterIsInTrash is_trashed) = is_trashed
    unsafeDocumentFilterIsTrashed _ = $unexpectedError "unsafeDocumentFilterIsTrashed"

unjsonDocumentFilterByText:: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterByText = pure DocumentFilterByText
  <*  fieldReadonly "filter_by" filterType "Type of filter"
  <*> field "text" unsafeDocumentFilterText "Text to filter on"
  where
    unsafeDocumentFilterText :: DocumentFilter ->  Text
    unsafeDocumentFilterText (DocumentFilterByText text) = text
    unsafeDocumentFilterText _ = $unexpectedError "unsafeDocumentFilterText"

unjsonDocumentFilterCanBeSignedBy:: Ap (FieldDef DocumentFilter) DocumentFilter
unjsonDocumentFilterCanBeSignedBy = pure DocumentFilterCanBeSignedBy
  <*  fieldReadonly "filter_by" filterType "Type of filter"
  <*> field "user_id" unsafeDocumentFilterUserID "Id of user that can sign"
  where
    unsafeDocumentFilterUserID :: DocumentFilter ->  UserID
    unsafeDocumentFilterUserID (DocumentFilterCanBeSignedBy uid) = uid
    unsafeDocumentFilterUserID _ = $unexpectedError "unsafeDocumentFilterStatus"


toDocumentFilter :: UserID -> DocumentFilter -> [DF.DocumentFilter]
toDocumentFilter _ (DocumentFilterStatuses ss) = [DF.DocumentFilterStatuses ss]
toDocumentFilter _ (DocumentFilterTime (Just start) (Just end)) = [DF.DocumentFilterByTimeAfter start, DF.DocumentFilterByTimeBefore end]
toDocumentFilter _ (DocumentFilterTime Nothing (Just end)) = [DF.DocumentFilterByTimeBefore end]
toDocumentFilter _ (DocumentFilterTime (Just start) Nothing) = [DF.DocumentFilterByTimeAfter start]
toDocumentFilter _ (DocumentFilterTime Nothing Nothing) = []
toDocumentFilter _ (DocumentFilterTag name value) = [DF.DocumentFilterByTags [DocumentTag (unpack name) (unpack value)]]
toDocumentFilter uid (DocumentFilterIsAuthor) = [DF.DocumentFilterByAuthor uid]
toDocumentFilter _ (DocumentFilterIsAuthoredBy uid) = [DF.DocumentFilterByAuthor uid]
toDocumentFilter _ (DocumentFilterIsSignableOnPad) = [DF.DocumentFilterSignNowOnPad]
toDocumentFilter _ (DocumentFilterIsTemplate True)  = [DF.DocumentFilterTemplate]
toDocumentFilter _ (DocumentFilterIsTemplate False) = [DF.DocumentFilterSignable]
toDocumentFilter _ (DocumentFilterIsInTrash bool) = [DF.DocumentFilterDeleted bool]
toDocumentFilter _ (DocumentFilterByText text) = [DF.DocumentFilterByString (unpack text)]
toDocumentFilter _ (DocumentFilterCanBeSignedBy uid) = [DF.DocumentFilterByCanSign uid]
