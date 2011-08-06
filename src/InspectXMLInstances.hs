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
import API.Service.ServiceState (ServiceID(..))
import Doc.DocState
import Company.CompanyState
import DB.Types
import Kontra
import MinutesTime
import InspectXML
import Payments.PaymentsState
import Mails.MailsUtil
import KontraLink
import FlashMessage
import qualified Data.ByteString.UTF8 as BS

instance (InspectXML a, Show a) => InspectXML [a] where
    inspectXML l = "[" ++ (concatMap (\s -> (inspectXML s) ++ "<BR/>") l) ++ "]"

instance (InspectXML a, Show a) => InspectXML (Maybe a) where
    inspectXML Nothing = "Nothing"
    inspectXML (Just x) = inspectXML x

instance (InspectXML a, InspectXML b, InspectXML c, Show a, Show b, Show c) => InspectXML (a, b, c) where
    inspectXML (a, b, c) = "(" ++ inspectXML a ++"," ++ inspectXML b ++ "," ++ inspectXML c ++ ")"

$(deriveInspectXML ''Document)
$(deriveInspectXML ''AuthorAttachment)
$(deriveInspectXML ''SignatoryAttachment)
$(deriveInspectXML ''FieldDefinition)
$(deriveInspectXML ''FieldPlacement)
$(deriveInspectXML ''User)
$(deriveInspectXML ''SignatoryLink)
$(deriveInspectXML ''SignatoryDetails)


--Link creating types
instance InspectXML DocumentID where
    inspectXML x = "<a href='/dave/document/" ++ show x ++ "'>"  ++ show x ++"</a>"
instance InspectXML SignatoryLinkID where
    inspectXML x =  "<a href='/dave/signatorylink/" ++ show x ++ "'>"  ++ show x ++"</a>"
instance InspectXML UserID where
    inspectXML x =  "<a href='/dave/user/" ++ show x ++ "'>"  ++ show x ++"</a>"
instance InspectXML File where
    inspectXML file= "<a href='" ++ (inspectXML $ LinkFile (fileid file) (filename file)) ++"'>" ++ show (fileid file)++ "/" ++ inspectXML (filename file) ++"</a>"

instance InspectXML DocumentLogEntry where
    inspectXML (DocumentLogEntry time text) = show time ++ ": " ++ inspectXML text

--Standard classes - we will just call show with some escaping
instance InspectXML String where
instance InspectXML BS.ByteString where
  inspectXML = inspectXML . BS.toString
instance InspectXML Bool where
instance InspectXML Int where
instance InspectXML Author where
instance InspectXML Friend where
instance InspectXML Inviter where
instance InspectXML ServiceID where
instance InspectXML DefaultMainSignatory where
instance InspectXML Integer where
instance InspectXML SignOrder where
instance InspectXML SignatoryRole where
instance InspectXML DocumentStatus where
instance InspectXML CSVUpload where
instance InspectXML DocumentType where
instance InspectXML DocumentFunctionality where
instance InspectXML ChargeMode where
instance InspectXML DocumentTag where
instance InspectXML DocumentUI where
instance InspectXML TimeoutTime where
instance InspectXML SignInfo where
instance InspectXML DocumentHistoryEntry where
instance InspectXML MagicHash where
instance InspectXML Signatory where
instance InspectXML MinutesTime where
instance InspectXML SupervisorID where
instance InspectXML Password where
instance InspectXML FlashMessage where
instance InspectXML Email where
instance InspectXML SignupMethod where
instance InspectXML MailsDeliveryStatus where
instance InspectXML UserInfo where
instance InspectXML UserMailAPI where
instance InspectXML UserSettings where
instance InspectXML UserPaymentPolicy where
instance InspectXML UserPaymentAccount where
instance InspectXML IdentificationType where
instance InspectXML CancelationReason where
instance InspectXML SignatureProvider where
instance InspectXML SignatureInfo where
instance InspectXML InviteInfo where
instance InspectXML LoginInfo where
instance InspectXML Company where
instance InspectXML CompanyID where
instance InspectXML DocumentSharing where
instance InspectXML KontraLink where

