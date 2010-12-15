{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  InspectXMLInstances
--
-- Instances of InspectXML for SkrivaPa data-types. Moved to this module to use auto-derivation.
-- If someone know how to join InspectXMLInstances with InspectXML he should do it. 
-----------------------------------------------------------------------------

module InspectXMLInstances() where
import DocState
import HSP
import User
import Misc
import MinutesTime
import InspectXML
import UserState
import Payments.PaymentsState

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
    
--Standard instances for other data types (based on show)
instance InspectXML File where
    inspectXML = asChild . show
instance InspectXML DocumentStatus where
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

instance InspectXML UserInfo where
    inspectXML = asChild . show 

instance InspectXML UserSettings where
    inspectXML = asChild . show 

instance InspectXML UserPaymentPolicy where
    inspectXML = asChild . show 

instance InspectXML UserPaymentAccount where
    inspectXML = asChild . show 
                     