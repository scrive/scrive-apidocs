{-# OPTIONS_GHC -F -pgmFtrhsx #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  InspectXMLInstances
--
-- Instances of InspectXML for SkrivaPa data-types. Moved to this module to use auto-derivation.
-- If someone know how to join InspectXMLInstances with InspectXML he should do it. 
-----------------------------------------------------------------------------

module InspectXMLInstances() where
import Doc.DocState
import HSP
import Kontra
import Misc
import MinutesTime
import InspectXML
import User.UserState
import Payments.PaymentsState
import Mails.MailsUtil
import KontraLink
import qualified Data.ByteString.UTF8 as BS

--Complex types, usualy big that should be derived automatically
$(deriveInspectXML ''Document)

$(deriveInspectXML ''FieldDefinition)

$(deriveInspectXML ''FieldPlacement)
    
$(deriveInspectXML ''User)

$(deriveInspectXML ''SignatoryLink)

$(deriveInspectXML ''SignatoryDetails)


--Link creating types
instance InspectXML DocumentID where
    inspectXML x = asChild <a href=("/dave/document/" ++ show x)><% show x %></a>
instance InspectXML SignatoryLinkID where
    inspectXML x = asChild <a href=("/dave/signatorylink/" ++ show x)><% show x %></a>  
instance InspectXML UserID where
    inspectXML x = asChild <a href=("/dave/user/" ++ show x)><% show x %></a>

--UnWrappers, usualy when we want to use above but our type is wraped    
instance InspectXML Author where
    inspectXML (Author x) = inspectXML x     
instance InspectXML Friend where
    inspectXML (Friend x) = inspectXML x
instance InspectXML Inviter where
    inspectXML (Inviter x) = inspectXML x
 
 -- old; should be removed
instance InspectXML DefaultMainSignatory where
    inspectXML (DefaultMainSignatory x) = inspectXML x
    
--Standard instances for other data types (based on show)
instance InspectXML File where
    inspectXML (File 
          { fileid
          , filename
          , filestorage
          })= asChild <a href=(show $ LinkFile fileid filename)><% show fileid ++ "/" ++ BS.toString filename %></a>
                              
instance InspectXML DocumentStatus where
    inspectXML = asChild . show
instance InspectXML DocumentType where
    inspectXML = asChild . show
instance InspectXML ChargeMode where
    inspectXML = asChild . show
instance InspectXML TimeoutTime where
    inspectXML = asChild . show
instance InspectXML SignInfo where
    inspectXML = asChild . show
instance InspectXML DocumentHistoryEntry where
    inspectXML = asChild . show
instance InspectXML MagicHash where
    inspectXML = asChild . show
instance InspectXML Signatory where
    inspectXML = asChild . show
instance InspectXML MinutesTime where
    inspectXML = asChild . show  
instance InspectXML SupervisorID where
    inspectXML = asChild . show  
instance InspectXML Password where
    inspectXML = asChild . show
instance InspectXML FlashMessage where
    inspectXML = asChild . show
instance InspectXML Email where
    inspectXML = asChild . show
    
instance InspectXML MailsDeliveryStatus where
    inspectXML = asChild . show
    
instance InspectXML UserInfo where
    inspectXML = asChild . show 

instance InspectXML UserSettings where
    inspectXML = asChild . show 

instance InspectXML UserPaymentPolicy where
    inspectXML = asChild . show 

instance InspectXML UserPaymentAccount where
    inspectXML = asChild . show 
                     
instance InspectXML IdentificationType where
    inspectXML = asChild . show

instance InspectXML SignatureProvider where
    inspectXML = asChild . show

instance InspectXML SignatureInfo where
    inspectXML = asChild . show

instance InspectXML InviteInfo where
    inspectXML = asChild . show
