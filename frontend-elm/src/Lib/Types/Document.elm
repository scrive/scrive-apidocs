module Lib.Types.Document exposing (..)

import Lib.Types.ID exposing (ID)
import Lib.Types.SignatoryLink exposing (SignatoryLink)
import Lib.Types.UrlString exposing (UrlString)
import Time


type alias UTCTime =
    Time.Posix


type DocumentStatus
    = Preparation
    | Pending
    | Closed
    | Canceled
    | Timedout
    | Rejected
    | DocumentError



-- We use `String` rather than explicit enum to reduce the compatibility
-- burden.


type Lang
    = Lang String



-- the alias is used for parsing


type Document
    = Document DocumentRecord


type alias DocumentRecord =
    { id : ID Document
    , title : String
    , parties : List SignatoryLink

    -- , file : Maybe File
    -- , sealedFile : Maybe File
    -- , authorAttachments : List AuthorAttachment
    , ctime : UTCTime
    , mtime : UTCTime

    -- , timeoutTime : Maybe UTCTime
    -- , autoRemindTime : Maybe UTCTime
    , status : DocumentStatus

    -- , daysToSign : Int
    -- , daysToRemind : Maybe Int
    -- , displayOptions : DocumentDisplayOptions
    -- , invitationMessage : String
    -- , confirmationMessage : String
    -- , smsInvitationMessage : String
    -- , smsConfirmationMessage : String
    , lang : Lang

    -- , apiCallbackUrl : Maybe String
    -- , objectVersion : Int
    -- , accessToken : Maybe MagicHash
    -- , timezone : TimeZone
    -- , tags : List DocumentTag
    , isTemplate : Bool

    -- , isSaved : Bool
    -- , folderId : ID Folder
    , isShared : Bool --- ro

    -- , isTrashed : Bool
    -- , isDeleted : Bool
    -- , viewer : DocumentViewer
    , shareableLink : Maybe UrlString

    -- , templateId : Maybe (ID Document)
    -- , fromShareableLink : Bool
    -- , experimentalFeatures : {
    --     userGroupToImpersonateForEid : Maybe (ID UserGroup)
    --   }
    }



{-
   type File = File {
       id : ID File
     , name : String
     }

   type AuthorAttachment = AuthorAttachment {
       name : String
     , required : Bool
     , addToSealedFile : Bool
     , fileId : ID File
     }

   type DocumentDisplayOptions = DocumentDisplayOptions {
       showHeader : Bool
     , showPdfDownload : Bool
     , showRejectOption : Bool
     , allowRejectReason : Bool
     , showFooter : Bool
     , documentIsReceipt : Bool
     , showArrow : Bool
     }


   type Lang = LANG_SV | LANG_EN | LANG_DE | LANG_FR | LANG_IT | LANG_ES | LANG_PT
             | LANG_NL | LANG_DA | LANG_NO | LANG_GR | LANG_FI | LANG_IS | LANG_ET
             | LANG_LV | LANG_LT | LANG_CS | LANG_PL | LANG_HU

   type MagicHash = MagicHash Int

   type TimeZone = TimeZone String  -- http://en.wikipedia.org/wiki/ListOfTzDatabaseTime_zones

   type DocumentTag = DocumentTag {
       name : String
     , value : String
     }

   type Folder = Folder {
       id : ID Folder
     , parentId : Maybe (ID Folder)
     , name : String
     }

   type DocumentViewerRole
     = Signatory
     | CompanyAdmin
     | CompanyShared

   type DocumentViewer = DocumentViewer {
       role : DocumentViewerRole
     , signatoryId : ID SignatoryLink
     }
-}
