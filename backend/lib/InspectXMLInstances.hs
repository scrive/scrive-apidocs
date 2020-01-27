{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  InspectXMLInstances
--
-- Instances of InspectXML for Scrive data types. Moved to this module
-- to use auto-derivation. If someone knows how to join
-- InspectXMLInstances with InspectXML he should do it.
-----------------------------------------------------------------------------

module InspectXMLInstances (ExtraDocument(..)) where

import Data.Int
import Text.JSON
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Set as S
import qualified Data.Text as T

import BrandedDomain.BrandedDomainID
import DB.TimeZoneName
import Doc.DocStateData
import Doc.DocumentID
import Doc.SealStatus (SealStatus)
import Doc.SignatoryConsentQuestionID
import Doc.SignatoryLinkID
import Doc.Types.SignatoryAccessToken
import File.File
import File.FileID
import FlashMessage
import Folder.Types
import InspectXML
import IPAddress
import KontraLink
import LoginAuth.LoginAuthMethod
import MagicHash (MagicHash)
import MinutesTime
import User.Email
import User.History.Model
import User.Model
import UserGroup.Types
import Utils.String

instance (InspectXML a, Show a) => InspectXML [a] where
  inspectXML l =
    "<ul>" <> (T.concat $ fmap (\s -> "<li>" <> (inspectXML s) <> "</li>") l) <> "</ul>"

instance (InspectXML a, Show a) => InspectXML (Maybe a) where
  inspectXML Nothing  = "Nothing"
  inspectXML (Just x) = inspectXML x

instance (InspectXML a, InspectXML b, Show a, Show b) => InspectXML (a, b) where
  inspectXML (a, b) = "(" <> inspectXML a <> "," <> inspectXML b <> ")"

instance ( InspectXML a, InspectXML b, InspectXML c
         , Show a, Show b, Show c ) =>
         InspectXML (a, b, c) where
  inspectXML (a, b, c) =
    "(" <> inspectXML a <> "," <> inspectXML b <> "," <> inspectXML c <> ")"

$(deriveInspectXML ''MainFile)
$(deriveInspectXML ''Document)
$(deriveInspectXML ''AuthorAttachment)
$(deriveInspectXML ''HighlightedPage)
$(deriveInspectXML ''SignatoryAttachment)
$(deriveInspectXML ''SignatoryRole)
$(deriveInspectXML ''FieldPlacement)
$(deriveInspectXML ''TipSide)
$(deriveInspectXML ''User)
$(deriveInspectXML ''UserHistory)
$(deriveInspectXML ''UserHistoryEvent)
$(deriveInspectXML ''SignatoryLink)
$(deriveInspectXML ''PlacementAnchor)
$(deriveInspectXML ''LoginAuthMethod)
$(deriveInspectXML ''SignatoryConsentQuestion)
$(deriveInspectXML ''SignatoryAccessToken)
$(deriveInspectXML ''SignatoryAccessTokenReason)

newtype ExtraDocument = ExtraDocument String
  deriving Show

instance InspectXML SignatoryField where
  inspectXML field =
    showt (fieldIdentity field)
      <> " "
      <> value
      <> ", "
      <> (if fieldIsObligatory field then "obligatory, " else "optional, ")
      <> (if fieldShouldBeFilledBySender field then "filled by sender, " else "")
      <> (if (fieldEditableBySignatory field == Just True)
           then "editable by signatory, "
           else ""
         )
      <> "<br/>placements: "
      <> inspectXML (fieldPlacements field)
    where
      value = case (fieldType field) of
        SignatureFT -> inspectXML (fieldFileValue field)
        CheckboxFT ->
          if (fromJust (fieldBoolValue field)) then "checked" else "not checked"
        RadioGroupFT -> inspectXML $ case fieldTextValue field of
          Nothing -> "<not picked>"
          Just s  -> s
        _ -> inspectXML (fromJust (fieldTextValue field))

instance InspectXML SignatoryConsentQuestionID where
  inspectXML = showt

-- this must be manually updated to match Document instance
instance InspectXML ExtraDocument
 where
  inspectXML (ExtraDocument callbackResult) = table "DocumentExtra"
    $ T.concat [table "API Callback result" $ inspectXML callbackResult]

--Link creating types
instance InspectXML DocumentID where
  inspectXML x = "<a href='/dave/document/" <> showt x <> "/'>" <> showt x <> "</a>"
instance InspectXML SignatoryLinkID where
  inspectXML x = "<a href='" <> showt x <> "'>" <> showt x <> "</a>"
instance InspectXML UserID where
  inspectXML x = "<a href='/dave/user/" <> showt x <> "'>" <> showt x <> "</a>"
instance InspectXML File where
  inspectXML file =
    "<a href='"
      <> (inspectXML $ LinkDaveFile (fileid file) (filename file))
      <> "'>"
      <> showt (fileid file)
      <> "/"
      <> inspectXML (filename file)
      <> "</a> "
      <> fileAccessLogged
instance InspectXML FileID where
  inspectXML fileid =
    "<a href='"
      <> (inspectXML $ LinkDaveFile fileid (showt fileid))
      <> "'>"
      <> showt fileid
      <> "</a> "
      <> fileAccessLogged

fileAccessLogged :: Text
fileAccessLogged = "(<strong style='color: red'>ACCESS TO THIS FILE IS LOGGED</strong>)"

instance InspectXML (S.Set DocumentTag) where
  inspectXML = inspectXML . S.toList

instance InspectXML BrandedDomainID where
  inspectXML x =
    "<a href='/adminonly/brandeddomain/" <> showt x <> "'>" <> showt x <> "</a>"

instance {-# OVERLAPPING #-} InspectXML String where
  inspectXML str = "\"" <> escapeString (showt str) <> "\""

instance InspectXML B.ByteString where
  inspectXML = inspectXML . BU.toString

--Standard classes - we will just call showt with some escaping
instance InspectXML Bool where
instance InspectXML Char where
instance InspectXML Text where
instance InspectXML Int where
instance InspectXML Int16 where
instance InspectXML Int32 where
instance InspectXML Int64 where
instance InspectXML Integer where
instance InspectXML Float where
instance InspectXML Double where
instance InspectXML Rational where
instance InspectXML SignOrder where
instance InspectXML DocumentStatus where
instance InspectXML CSVUpload where
instance InspectXML DocumentType where
instance InspectXML DocumentTag where
instance InspectXML SignInfo where
instance InspectXML MagicHash where
instance InspectXML UTCTime where
instance InspectXML Password where
instance InspectXML FlashMessage where
instance InspectXML Email where
instance InspectXML SignupMethod where
instance InspectXML DeliveryStatus where
instance InspectXML UserInfo where
instance InspectXML UserSettings where
instance InspectXML AuthenticationToViewMethod where
instance InspectXML AuthenticationToSignMethod where
instance InspectXML DeliveryMethod where
instance InspectXML ConfirmationDeliveryMethod where
instance InspectXML NotificationDeliveryMethod where
instance InspectXML PlacementID where
instance InspectXML DocumentSharing where
instance InspectXML KontraLink where
instance InspectXML FieldType where
instance InspectXML Lang where
instance InspectXML IPAddress where
instance InspectXML UserHistoryEventType where
instance InspectXML JSValue where
instance InspectXML StatusClass where
instance InspectXML SealStatus
instance InspectXML TimeZoneName
instance InspectXML UserGroupID
instance InspectXML UserGroup
instance InspectXML Folder
instance InspectXML FolderID
