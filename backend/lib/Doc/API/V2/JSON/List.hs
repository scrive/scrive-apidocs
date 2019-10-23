module Doc.API.V2.JSON.List (
  toDocumentSorting
, toDocumentFilter
, defaultDocumentAPISort
) where

import Data.Unjson
import qualified Control.Applicative.Free as AltF

import DB
import Doc.API.V2.JSON.Misc
import Doc.API.V2.JSON.Utils
import Doc.DocStateData
import Doc.Model.OrderBy
import MinutesTime
import User.UserID
import qualified Doc.Model.Filter as DF

-- All sorting and filtering types defined in this module are internal
-- to API V2.  Sorting and filtering used by API is different then
-- sorting and filtering defined in Doc.Model.

defaultDocumentAPISort :: [DocumentAPISort]
defaultDocumentAPISort = [DocumentAPISort DocumentAPISortTime DocumentAPISortDesc]

-- Note that types defined here aren't even exported, since only
-- Unjson instance is used, and result of parsing is converted
-- immediately to sorting and filtering defined in Doc.Model

data DocumentAPISort = DocumentAPISort DocumentAPISortOn DocumentAPISortOrder

data DocumentAPISortOn = DocumentAPISortStatus | DocumentAPISortTitle | DocumentAPISortTime| DocumentAPISortAuthor deriving Eq
data DocumentAPISortOrder = DocumentAPISortAsc | DocumentAPISortDesc  deriving Eq

instance Unjson DocumentAPISortOrder where
  unjsonDef = unjsonEnumBy
    "DocumentAPISortOrder"
    [(DocumentAPISortAsc, "ascending"), (DocumentAPISortDesc, "descending")]

instance Unjson DocumentAPISortOn where
  unjsonDef = unjsonEnumBy
    "DocumentAPISortOn"
    [ (DocumentAPISortStatus, "status")
    , (DocumentAPISortTitle , "title")
    , (DocumentAPISortTime  , "mtime")
    , (DocumentAPISortAuthor, "author")
    ]

instance Unjson DocumentAPISort where
  unjsonDef =
    objectOf
      $   pure DocumentAPISort
      <*> field "sort_by" (\(DocumentAPISort v _) -> v) "How documents should be sorted"
      <*> fieldDef "order"
                   DocumentAPISortAsc
                   (\(DocumentAPISort _ o) -> o)
                   "Descending or ascending sorting"


toDocumentSorting :: DocumentAPISort -> AscDesc DocumentOrderBy
toDocumentSorting (DocumentAPISort DocumentAPISortStatus DocumentAPISortAsc) =
  Asc DocumentOrderByStatus
toDocumentSorting (DocumentAPISort DocumentAPISortTitle DocumentAPISortAsc) =
  Asc DocumentOrderByTitle
toDocumentSorting (DocumentAPISort DocumentAPISortTime DocumentAPISortAsc) =
  Asc DocumentOrderByMTime
toDocumentSorting (DocumentAPISort DocumentAPISortAuthor DocumentAPISortAsc) =
  Asc DocumentOrderByAuthor
toDocumentSorting (DocumentAPISort DocumentAPISortStatus DocumentAPISortDesc) =
  Desc DocumentOrderByStatus
toDocumentSorting (DocumentAPISort DocumentAPISortTitle DocumentAPISortDesc) =
  Desc DocumentOrderByTitle
toDocumentSorting (DocumentAPISort DocumentAPISortTime DocumentAPISortDesc) =
  Desc DocumentOrderByMTime
toDocumentSorting (DocumentAPISort DocumentAPISortAuthor DocumentAPISortDesc) =
  Desc DocumentOrderByAuthor

data DocumentAPIFilter = DocumentAPIFilterStatuses [DocumentStatus]
                    | DocumentAPIFilterTime (Maybe UTCTime) (Maybe UTCTime)
                    | DocumentAPIFilterTag Text Text
                    | DocumentAPIFilterIsAuthor
                    | DocumentAPIFilterIsAuthoredBy UserID
                    | DocumentAPIFilterIsSignableOnPad
                    | DocumentAPIFilterIsTemplate
                    | DocumentAPIFilterIsNotTemplate
                    | DocumentAPIFilterIsInTrash
                    | DocumentAPIFilterIsNotInTrash
                    | DocumentAPIFilterByText Text
                    | DocumentAPIFilterCanBeSignedBy UserID


filterType :: DocumentAPIFilter -> Text
filterType (DocumentAPIFilterStatuses _     ) = "status"
filterType (DocumentAPIFilterTime _ _       ) = "mtime"
filterType (DocumentAPIFilterTag  _ _       ) = "tag"
filterType (DocumentAPIFilterIsAuthor       ) = "is_author"
filterType (DocumentAPIFilterIsAuthoredBy _ ) = "author"
filterType (DocumentAPIFilterIsSignableOnPad) = "is_signable_on_pad"
filterType (DocumentAPIFilterIsTemplate     ) = "is_template"
filterType (DocumentAPIFilterIsNotTemplate  ) = "is_not_template"
filterType (DocumentAPIFilterIsInTrash      ) = "is_in_trash"
filterType (DocumentAPIFilterIsNotInTrash   ) = "is_not_in_trash"
filterType (DocumentAPIFilterByText        _) = "text"
filterType (DocumentAPIFilterCanBeSignedBy _) = "user_can_sign"

instance Unjson DocumentAPIFilter where
  unjsonDef =
    disjointUnionOf "filter_by"
      $   filterMatch
      <$> [ (DocumentAPIFilterStatuses []         , unjsonDocumentAPIFilterStatuses)
          , (DocumentAPIFilterTime Nothing Nothing, unjsonDocumentAPIFilterTime)
          , (DocumentAPIFilterTag "" ""           , unjsonDocumentAPIFilterTag)
          , (DocumentAPIFilterIsAuthor            , unjsonDocumentAPIFilterIsAuthor)
          , ( DocumentAPIFilterIsAuthoredBy (unsafeUserID 0)
            , unjsonDocumentAPIFilterIsAuthoredBy
            )
          , (DocumentAPIFilterIsSignableOnPad, unjsonDocumentAPIFilterIsSignableOnPad)
          , (DocumentAPIFilterIsTemplate     , unjsonDocumentAPIFilterIsTemplate)
          , (DocumentAPIFilterIsNotTemplate  , unjsonDocumentAPIFilterIsNotTemplate)
          , (DocumentAPIFilterIsInTrash      , unjsonDocumentAPIFilterIsInTrash)
          , (DocumentAPIFilterIsNotInTrash   , unjsonDocumentAPIFilterIsNotInTrash)
          , (DocumentAPIFilterByText ""      , unjsonDocumentAPIFilterByText)
          , ( DocumentAPIFilterCanBeSignedBy (unsafeUserID 0)
            , unjsonDocumentAPIFilterCanBeSignedBy
            )
          ]
    where
      filterMatch
        :: (DocumentAPIFilter, AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter)
        -> ( Text
           , DocumentAPIFilter -> Bool
           , AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
           )
      filterMatch (df, a) = (filterType df, \f -> filterType df == filterType f, a)

unjsonDocumentAPIFilterStatuses :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterStatuses =
  pure DocumentAPIFilterStatuses
    <*  fieldReadonly "filter_by" filterType "Type of filter"
    <*> fieldBy "statuses"
                unsafeDocumentAPIFilterStatuses
                "Statuses to filter on"
                (arrayOf unjsonDocumentStatus)
  where
    unsafeDocumentAPIFilterStatuses :: DocumentAPIFilter -> [DocumentStatus]
    unsafeDocumentAPIFilterStatuses (DocumentAPIFilterStatuses fs) = fs
    unsafeDocumentAPIFilterStatuses _ = unexpectedError "unsafeDocumentAPIFilterStatus"

unjsonDocumentAPIFilterTime :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterTime =
  pure DocumentAPIFilterTime
    <*  fieldReadonly "filter_by" filterType "Type of filter"
    <*> fieldOpt "start_time"
                 unsafeDocumentAPIFilterStartTime
                 "Only documents after start time"
    <*> fieldOpt "end_time"
                 unsafeDocumentAPIFilterEndTime
                 "Only documents before end time"
  where
    unsafeDocumentAPIFilterStartTime :: DocumentAPIFilter -> Maybe UTCTime
    unsafeDocumentAPIFilterStartTime (DocumentAPIFilterTime s _) = s
    unsafeDocumentAPIFilterStartTime _ =
      unexpectedError "unsafeDocumentAPIFilterStartTime"
    unsafeDocumentAPIFilterEndTime :: DocumentAPIFilter -> Maybe UTCTime
    unsafeDocumentAPIFilterEndTime (DocumentAPIFilterTime _ e) = e
    unsafeDocumentAPIFilterEndTime _ = unexpectedError "unsafeDocumentAPIFilterEndTime"

unjsonDocumentAPIFilterTag :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterTag =
  pure DocumentAPIFilterTag
    <*  fieldReadonly "filter_by" filterType "Type of filter"
    <*> field "name"  unsafeDocumentAPIFilterTagName  "Name of tag to filter on"
    <*> field "value" unsafeDocumentAPIFilterTagValue "Value of such tag"
  where
    unsafeDocumentAPIFilterTagName :: DocumentAPIFilter -> Text
    unsafeDocumentAPIFilterTagName (DocumentAPIFilterTag n _) = n
    unsafeDocumentAPIFilterTagName _ = unexpectedError "unsafeDocumentAPIFilterTagName"
    unsafeDocumentAPIFilterTagValue :: DocumentAPIFilter -> Text
    unsafeDocumentAPIFilterTagValue (DocumentAPIFilterTag _ v) = v
    unsafeDocumentAPIFilterTagValue _ = unexpectedError "unsafeDocumentAPIFilterTagValue"

unjsonDocumentAPIFilterIsAuthor :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterIsAuthor =
  pure DocumentAPIFilterIsAuthor <* fieldReadonly "filter_by" filterType "Type of filter"

unjsonDocumentAPIFilterIsAuthoredBy
  :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterIsAuthoredBy =
  pure DocumentAPIFilterIsAuthoredBy
    <*  fieldReadonly "filter_by" filterType "Type of filter"
    <*> field "user_id" unsafeDocumentAPIFilterUserID "Id of author"
  where
    unsafeDocumentAPIFilterUserID :: DocumentAPIFilter -> UserID
    unsafeDocumentAPIFilterUserID (DocumentAPIFilterIsAuthoredBy uid) = uid
    unsafeDocumentAPIFilterUserID _ = unexpectedError "unsafeDocumentAPIFilterStatus"

unjsonDocumentAPIFilterIsSignableOnPad
  :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterIsSignableOnPad =
  pure DocumentAPIFilterIsSignableOnPad
    <* fieldReadonly "filter_by" filterType "Type of filter"


unjsonDocumentAPIFilterIsTemplate
  :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterIsTemplate = pure DocumentAPIFilterIsTemplate
  <* fieldReadonly "filter_by" filterType "Type of filter"

unjsonDocumentAPIFilterIsNotTemplate
  :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterIsNotTemplate =
  pure DocumentAPIFilterIsNotTemplate
    <* fieldReadonly "filter_by" filterType "Type of filter"

unjsonDocumentAPIFilterIsInTrash :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterIsInTrash =
  pure DocumentAPIFilterIsInTrash <* fieldReadonly "filter_by" filterType "Type of filter"

unjsonDocumentAPIFilterIsNotInTrash
  :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterIsNotInTrash = pure DocumentAPIFilterIsNotInTrash
  <* fieldReadonly "filter_by" filterType "Type of filter"

unjsonDocumentAPIFilterByText :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterByText =
  pure DocumentAPIFilterByText
    <*  fieldReadonly "filter_by" filterType "Type of filter"
    <*> field "text" unsafeDocumentAPIFilterText "Text to filter on"
  where
    unsafeDocumentAPIFilterText :: DocumentAPIFilter -> Text
    unsafeDocumentAPIFilterText (DocumentAPIFilterByText text) = text
    unsafeDocumentAPIFilterText _ = unexpectedError "unsafeDocumentAPIFilterText"

unjsonDocumentAPIFilterCanBeSignedBy
  :: AltF.Ap (FieldDef DocumentAPIFilter) DocumentAPIFilter
unjsonDocumentAPIFilterCanBeSignedBy =
  pure DocumentAPIFilterCanBeSignedBy
    <*  fieldReadonly "filter_by" filterType "Type of filter"
    <*> field "user_id" unsafeDocumentAPIFilterUserID "Id of user that can sign"
  where
    unsafeDocumentAPIFilterUserID :: DocumentAPIFilter -> UserID
    unsafeDocumentAPIFilterUserID (DocumentAPIFilterCanBeSignedBy uid) = uid
    unsafeDocumentAPIFilterUserID _ = unexpectedError "unsafeDocumentAPIFilterStatus"


toDocumentFilter :: UserID -> DocumentAPIFilter -> [DF.DocumentFilter]
toDocumentFilter _ (DocumentAPIFilterStatuses ss) = [DF.DocumentFilterStatuses ss]
toDocumentFilter _ (DocumentAPIFilterTime (Just start) (Just end)) =
  [DF.DocumentFilterByTimeAfter start, DF.DocumentFilterByTimeBefore end]
toDocumentFilter _ (DocumentAPIFilterTime Nothing (Just end)) =
  [DF.DocumentFilterByTimeBefore end]
toDocumentFilter _ (DocumentAPIFilterTime (Just start) Nothing) =
  [DF.DocumentFilterByTimeAfter start]
toDocumentFilter _ (DocumentAPIFilterTime Nothing Nothing) = []
toDocumentFilter _ (DocumentAPIFilterTag name value) =
  [DF.DocumentFilterByTags [DocumentTag name value]]
toDocumentFilter uid (DocumentAPIFilterIsAuthor        ) = [DF.DocumentFilterByAuthor uid]
toDocumentFilter _   (DocumentAPIFilterIsAuthoredBy uid) = [DF.DocumentFilterByAuthor uid]
toDocumentFilter uid (DocumentAPIFilterIsSignableOnPad) =
  [DF.DocumentFilterByAuthor uid, DF.DocumentFilterSignNowOnPad]
toDocumentFilter _ (DocumentAPIFilterIsTemplate   ) = [DF.DocumentFilterTemplate]
toDocumentFilter _ (DocumentAPIFilterIsNotTemplate) = [DF.DocumentFilterSignable]
toDocumentFilter _ (DocumentAPIFilterIsInTrash    ) = [DF.DocumentFilterDeleted True]
toDocumentFilter _ (DocumentAPIFilterIsNotInTrash ) = [DF.DocumentFilterDeleted False]
toDocumentFilter _ (DocumentAPIFilterByText text) = [DF.processSearchStringToFilter text]
toDocumentFilter _ (DocumentAPIFilterCanBeSignedBy uid) =
  [DF.DocumentFilterByCanSign uid]
