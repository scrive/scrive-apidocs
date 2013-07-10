module Doc.DocProcess (
  DocProcessInfo(..),
  getValueForProcess,
  renderTemplateForProcess,
  renderLocalTemplateForProcess)
where

import Doc.DocStateData
import Text.StringTemplates.Templates
import Templates
import User.Lang

class HasProcess a where
  getProcess :: a -> Maybe DocProcessInfo

  getValueForProcess :: a -> (DocProcessInfo -> b) -> Maybe b
  getValueForProcess doctype fieldname =
    fmap fieldname (getProcess doctype)

  renderTemplateForProcess :: TemplatesMonad m => a -> (DocProcessInfo -> String) -> Fields m () -> m String
  renderTemplateForProcess hasprocess fieldname fields =
    case getValueForProcess hasprocess fieldname of
      Just templatename -> renderTemplate templatename fields
      _ -> return ""


renderLocalTemplateForProcess :: (HasLang a, HasProcess a, TemplatesMonad m)
                                 => a
                                 -> (DocProcessInfo -> String)
                                 -> Fields m ()
                                 -> m String
renderLocalTemplateForProcess hasprocess fieldname fields =
  case getValueForProcess hasprocess fieldname of
    Just templatename -> renderLocalTemplate hasprocess templatename fields
    _ -> return ""

instance HasProcess DocumentType where
  getProcess (Signable Contract) = Just contractProcess
  getProcess (Template Contract) = Just contractProcess
  getProcess (Signable Offer) = Just offerProcess
  getProcess (Template Offer) = Just offerProcess
  getProcess (Signable Order) = Just orderProcess
  getProcess (Template Order) = Just orderProcess

instance HasProcess Document where
  getProcess = getProcess . documenttype

data DocProcessInfo =
  DocProcessInfo {
  -- process specific doc mail template names
    processmailclosed :: String
  , processmailreject :: String
  , processmailinvitationtosign :: String
  , processmailinvitationtosigndefaultheader :: String
  , processmailremindnotsigned :: String
  , processwhohadsignedinfoformail :: String

  -- process specific flash message templates
  , processflashmessagerestarted :: String
  , processflashmessageprolonged :: String

  -- process specific seal information
  , processsealingtext :: String
  }

contractProcess :: DocProcessInfo
contractProcess =
  DocProcessInfo {
  -- process specific doc mail template names
    processmailclosed= "mailContractClosed"
  , processmailreject = "mailRejectContractMail"
  , processmailinvitationtosign = "mailInvitationToSignContract"
  , processmailinvitationtosigndefaultheader = "mailInvitationToSignContractDefaultHeader"
  , processmailremindnotsigned = "remindMailNotSignedContract"
  , processwhohadsignedinfoformail = "whohadsignedcontractinfoformail"

  -- process specific flash messages
  , processflashmessagerestarted = "flashMessageContractRestarted"
  , processflashmessageprolonged = "flashMessageContractProlonged"

  -- process specific seal information
  , processsealingtext = "contractsealingtexts"

  }

offerProcess :: DocProcessInfo
offerProcess =
  DocProcessInfo {
  -- process specific doc mail template names
    processmailclosed = "mailOfferClosed"
  , processmailreject = "mailRejectOfferMail"
  , processmailinvitationtosign = "mailInvitationToSignOffer"
  , processmailinvitationtosigndefaultheader = "mailInvitationToSignOfferDefaultHeader"
  , processmailremindnotsigned= "remindMailNotSignedOffer"
  , processwhohadsignedinfoformail = "whohadsignedofferinfoformail"

  -- process specific flash messages
  , processflashmessagerestarted = "flashMessageOfferRestarted"
  , processflashmessageprolonged = "flashMessageOfferProlonged"

  -- process specific seal information
  , processsealingtext = "offersealingtexts"

  }

orderProcess :: DocProcessInfo
orderProcess =
  DocProcessInfo {
  -- process specific doc mail template names
    processmailclosed = "mailOrderClosed"
  , processmailreject = "mailRejectOrderMail"
  , processmailinvitationtosign = "mailInvitationToSignOrder"
  , processmailinvitationtosigndefaultheader = "mailInvitationToSignOrderDefaultHeader"
  , processmailremindnotsigned = "remindMailNotSignedOrder"
  , processwhohadsignedinfoformail = "whohadsignedorderinfoformail"

  -- process specific flash messages
  , processflashmessagerestarted = "flashMessageOrderRestarted"
  , processflashmessageprolonged = "flashMessageOrderProlonged"

  -- process specific seal information
  , processsealingtext = "ordersealingtexts"

  }
