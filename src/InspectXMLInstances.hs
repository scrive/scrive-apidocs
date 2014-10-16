{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  InspectXMLInstances
--
-- Instances of InspectXML for SkrivaPa data-types. Moved to this module to use auto-derivation.
-- If someone know how to join InspectXMLInstances with InspectXML he should do it.
-----------------------------------------------------------------------------

module InspectXMLInstances() where
import Data.Int
import Data.List
import Text.JSON
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as S

import API.APIVersion
import BrandedDomain.BrandedDomainID
import Company.Model
import DB.TimeZoneName
import Doc.DocStateData
import Doc.DocumentID
import Doc.SealStatus (SealStatus)
import Doc.SignatoryLinkID
import File.File
import File.FileID
import FlashMessage
import InspectXML
import IPAddress
import KontraLink
import MagicHash (MagicHash)
import MinutesTime
import User.Email
import User.History.Model
import User.Model
import Utils.String

instance (InspectXML a, Show a) => InspectXML [a] where
    inspectXML l = "<ul>" ++ (concatMap (\s -> "<li>" ++ (inspectXML s) ++ "</li>") l) ++ "</ul>"

instance (InspectXML a, Show a) => InspectXML (Maybe a) where
    inspectXML Nothing = "Nothing"
    inspectXML (Just x) = inspectXML x

instance (InspectXML a, InspectXML b, InspectXML c, Show a, Show b, Show c) => InspectXML (a, b, c) where
    inspectXML (a, b, c) = "(" ++ inspectXML a ++"," ++ inspectXML b ++ "," ++ inspectXML c ++ ")"

$(deriveInspectXML ''MainFile)
$(deriveInspectXML ''Document)
$(deriveInspectXML ''AuthorAttachment)
$(deriveInspectXML ''SignatoryAttachment)
$(deriveInspectXML ''FieldPlacement)
$(deriveInspectXML ''TipSide)
$(deriveInspectXML ''User)
$(deriveInspectXML ''UserHistory)
$(deriveInspectXML ''UserHistoryEvent)
$(deriveInspectXML ''SignatoryLink)
$(deriveInspectXML ''PlacementAnchor)

instance InspectXML SignatoryField where
  inspectXML field =
    (show $ sfType field) ++ " " ++ useImgDataURL (sfValue field) ++ ", " ++
    (if sfObligatory field then "obligatory, " else "optional, ") ++
    (if sfShouldBeFilledBySender field then "filled by sender, " else "") ++
    "<br/>placements: " ++ inspectXML (sfPlacements field)
    where
      useImgDataURL txt | "data:image/" `isPrefixOf` txt
                        = "<img style=\"height:100px; width=auto\" src=\"" ++ escapeString txt ++ "\">"
      useImgDataURL txt = inspectXML txt

--Link creating types
instance InspectXML DocumentID where
    inspectXML x = "<a href='/dave/document/" ++ show x ++ "/'>"  ++ show x ++"</a>"
instance InspectXML SignatoryLinkID where
    inspectXML x =  "<a href='" ++ show x ++ "'>" ++ show x ++"</a>"
instance InspectXML UserID where
    inspectXML x =  "<a href='/dave/user/" ++ show x ++ "'>"  ++ show x ++"</a>"
instance InspectXML File where
    inspectXML file = "<a href='" ++ (inspectXML $ LinkDaveFile (fileid file) (filename file)) ++"'>" ++ show (fileid file)++ "/" ++ inspectXML (filename file) ++"</a>"
instance InspectXML FileID where
    inspectXML fileid = "<a href='" ++ (inspectXML $ LinkDaveFile fileid (show fileid)) ++"'>" ++ show fileid ++ "</a>"
instance InspectXML (S.Set DocumentTag) where
  inspectXML = inspectXML . S.toList

instance InspectXML BrandedDomainID where
    inspectXML x = "<a href='/adminonly/brandeddomain/" ++ show x ++ "'>"  ++ show x ++"</a>"

instance InspectXML String where
  inspectXML str = "\"" ++ escapeString str ++ "\""

instance InspectXML BS.ByteString where
  inspectXML = inspectXML . BS.toString

--Standard classes - we will just call show with some escaping
instance InspectXML Bool where
instance InspectXML Char where
instance InspectXML Int where
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
instance InspectXML AuthenticationMethod where
instance InspectXML DeliveryMethod where
instance InspectXML ConfirmationDeliveryMethod where
instance InspectXML CancelationReason where
instance InspectXML SignatureProvider where
instance InspectXML SignatureInfo where
instance InspectXML Company where
instance InspectXML CompanyID where
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
instance InspectXML APIVersion
