module Doc.DocProcess (
  DocProcessInfo(..),
  getValueForProcess,
  renderTextForProcess,
  renderTemplateForProcess,
  renderLocalTextForProcess,
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

  renderTextForProcess :: TemplatesMonad m => a -> (DocProcessInfo -> String) -> m String
  renderTextForProcess hasprocess fieldname =
      renderTemplateForProcess hasprocess fieldname $ do return ()

renderLocalTemplateForProcess :: (HasLang a, HasProcess a, TemplatesMonad m)
                                 => a
                                 -> (DocProcessInfo -> String)
                                 -> Fields m ()
                                 -> m String
renderLocalTemplateForProcess hasprocess fieldname fields =
  case getValueForProcess hasprocess fieldname of
    Just templatename -> renderLocalTemplate hasprocess templatename fields
    _ -> return ""

renderLocalTextForProcess :: (HasLang a, HasProcess a, TemplatesMonad m)
                             => a
                             -> (DocProcessInfo -> String)
                             -> m String
renderLocalTextForProcess hasprocess fieldname =
  renderLocalTemplateForProcess hasprocess fieldname $ do return ()


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
    processmailcancelstandardheader :: String
  , processmailclosed :: String
  , processmailreject :: String
  , processmailinvitationtosign :: String
  , processmailinvitationtosigndefaultheader :: String
  , processmailsignedstandardheader :: String
  , processmailnotsignedstandardheader :: String
  , processmailremindnotsigned :: String
  , processmailconfirmbymailapi :: String
  , processwhohadsignedinfoformail :: String

  -- process specific flash message templates
  , processflashmessagerestarted :: String

  -- process specific modal templates
  , processmodalsendconfirmation :: String

  -- process specific seal information
  , processsealingtext :: String
  , processlasthisentry :: String
  , processinvitationsententry :: String
  , processseenhistentry :: String
  , processsignhistentry :: String

  -- doctexts templates
  , processpendingauthornotsignedinfoheader :: String
  , processpendingauthornotsignedinfotext :: String
  , processpendinginfotext :: String
  , processcancelledinfoheader :: String
  , processcancelledinfotext :: String
  , processsignedinfoheader :: String
  , processsignedinfotext :: String
  , processstatusinfotext :: String
  }

contractProcess :: DocProcessInfo
contractProcess =
  DocProcessInfo {
  -- process specific doc mail template names
    processmailcancelstandardheader = "mailCancelContractStandardHeader"
  , processmailclosed= "mailContractClosed"
  , processmailreject = "mailRejectContractMail"
  , processmailinvitationtosign = "mailInvitationToSignContract"
  , processmailinvitationtosigndefaultheader = "mailInvitationToSignContractDefaultHeader"
  , processmailsignedstandardheader = "remindMailSignedContractStandardHeader"
  , processmailnotsignedstandardheader = "remindMailNotSignedContractStandardHeader"
  , processmailremindnotsigned = "remindMailNotSignedContract"
  , processmailconfirmbymailapi = "mailMailAPIConfirmContract"
  , processwhohadsignedinfoformail = "whohadsignedcontractinfoformail"

  -- process specific flash messages
  , processflashmessagerestarted = "flashMessageContractRestarted"

  -- process specific modal templates
  , processmodalsendconfirmation = "modalContractCreated"

  -- process specific seal information
  , processsealingtext = "contractsealingtexts"
  , processlasthisentry = "contractLastHistEntry"
  , processinvitationsententry = "contractInvitationSentEntry"
  , processseenhistentry = "contractSeenHistEntry"
  , processsignhistentry = "contractSignHistEntry"

  -- doctexts templates
  , processpendingauthornotsignedinfoheader = "contractpendingauthornotsignedinfoheader"
  , processpendingauthornotsignedinfotext = "contractpendingauthornotsignedinfotext"
  , processpendinginfotext = "contractpendinginfotext"
  , processcancelledinfoheader = "contractcancelledinfoheader"
  , processcancelledinfotext = "contractcancelledinfotext"
  , processsignedinfoheader = "contractsignedinfoheader"
  , processsignedinfotext = "contractsignedinfotext"
  , processstatusinfotext = "contractstatusinfotext"
  }

offerProcess :: DocProcessInfo
offerProcess =
  DocProcessInfo {
  -- process specific doc mail template names
    processmailcancelstandardheader = "mailCancelOfferStandardHeader"
  , processmailclosed = "mailOfferClosed"
  , processmailreject = "mailRejectOfferMail"
  , processmailinvitationtosign = "mailInvitationToSignOffer"
  , processmailinvitationtosigndefaultheader = "mailInvitationToSignOfferDefaultHeader"
  , processmailsignedstandardheader = "remindMailSignedOfferStandardHeader"
  , processmailnotsignedstandardheader = "remindMailNotSignedOfferStandardHeader"
  , processmailremindnotsigned= "remindMailNotSignedOffer"
  , processmailconfirmbymailapi = "mailMailAPIConfirmOffer"
  , processwhohadsignedinfoformail = "whohadsignedofferinfoformail"

  -- process specific flash messages
  , processflashmessagerestarted = "flashMessageOfferRestarted"

  -- process specific modal templates
  , processmodalsendconfirmation = "modalOfferCreated"

  -- process specific seal information
  , processsealingtext = "offersealingtexts"
  , processlasthisentry = "offerLastHistEntry"
  , processinvitationsententry = "offerInvitationSentEntry"
  , processseenhistentry = "offerSeenHistEntry"
  , processsignhistentry = "offerSignHistEntry"

  -- doctexts templates
  , processpendingauthornotsignedinfoheader = "offerpendingauthornotsignedinfoheader"
  , processpendingauthornotsignedinfotext = "offerpendingauthornotsignedinfotext"
  , processpendinginfotext = "offerpendinginfotext"
  , processcancelledinfoheader = "offercancelledinfoheader"
  , processcancelledinfotext = "offercancelledinfotext"
  , processsignedinfoheader = "offersignedinfoheader"
  , processsignedinfotext = "offersignedinfotext"
  , processstatusinfotext = "offerstatusinfotext"

  }

orderProcess :: DocProcessInfo
orderProcess =
  DocProcessInfo {
  -- process specific doc mail template names
    processmailcancelstandardheader = "mailCancelOrderStandardHeader"
  , processmailclosed = "mailOrderClosed"
  , processmailreject = "mailRejectOrderMail"
  , processmailinvitationtosign = "mailInvitationToSignOrder"
  , processmailinvitationtosigndefaultheader = "mailInvitationToSignOrderDefaultHeader"
  , processmailsignedstandardheader = "remindMailSignedOrderStandardHeader"
  , processmailnotsignedstandardheader = "remindMailNotSignedOrderStandardHeader"
  , processmailremindnotsigned = "remindMailNotSignedOrder"
  , processmailconfirmbymailapi = "mailMailAPIConfirmOrder"
  , processwhohadsignedinfoformail = "whohadsignedorderinfoformail"

  -- process specific flash messages
  , processflashmessagerestarted = "flashMessageOrderRestarted"

  -- process specific modal templates
  , processmodalsendconfirmation = "modalOrderCreated"

  -- process specific seal information
  , processsealingtext = "ordersealingtexts"
  , processlasthisentry = "orderLastHistEntry"
  , processinvitationsententry = "orderInvitationSentEntry"
  , processseenhistentry = "orderSeenHistEntry"
  , processsignhistentry = "orderSignHistEntry"

  -- doctexts templates
  , processpendingauthornotsignedinfoheader = "orderpendingauthornotsignedinfoheader"
  , processpendingauthornotsignedinfotext = "orderpendingauthornotsignedinfotext"
  , processpendinginfotext = "orderpendinginfotext"
  , processcancelledinfoheader = "ordercancelledinfoheader"
  , processcancelledinfotext = "ordercancelledinfotext"
  , processsignedinfoheader = "ordersignedinfoheader"
  , processsignedinfotext = "ordersignedinfotext"
  , processstatusinfotext = "orderstatusinfotext"

  }
