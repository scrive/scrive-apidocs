{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind -Werror #-}
module Doc.DocProcess (
  DocProcessInfo(..),
  getValueForProcess,
  renderTextForProcess,
  renderTemplateForProcess)
where

import Doc.DocStateData
import Templates.Templates


class HasProcess a where
  getProcess :: a -> Maybe DocProcessInfo

  getValueForProcess :: a -> (DocProcessInfo -> b) -> b
  getValueForProcess doctype fieldname =
    case fmap fieldname (getProcess doctype) of
      Nothing -> error "there is no process"
      (Just val) -> val

  renderTemplateForProcess :: KontrakcjaTemplates -> a -> (DocProcessInfo -> String) -> Fields -> IO String
  renderTemplateForProcess templates hasprocess fieldname =
    let templatename = getValueForProcess hasprocess fieldname in
    renderTemplate templates templatename

  renderTextForProcess :: KontrakcjaTemplates -> a -> (DocProcessInfo -> String) -> IO String
  renderTextForProcess templates hasprocess fieldname = renderTemplateForProcess templates hasprocess fieldname $ do return ()

instance HasProcess DocumentType where
  getProcess (Signable Contract) = Just contractProcess
  getProcess (Template Contract) = Just contractProcess
  getProcess (Signable Offer) = Just offerProcess
  getProcess (Template Offer) = Just offerProcess
  getProcess (Signable Order) = Just orderProcess
  getProcess (Template Order) = Just orderProcess
  getProcess _ = Nothing

instance HasProcess Document where
  getProcess = getProcess . documenttype

data DocProcessInfo =
  DocProcessInfo {

  -- templates used in lots of different places
    processtitle :: String
  , processname :: String

  -- used when uploading
  , processuploadprompttext :: String
  , processuploadname :: String

  -- used in the design view
  , processadvancedview :: Bool
  , processauthorsend :: Bool
  , processvalidationchoiceforbasic :: Bool
  , processexpiryforbasic :: Bool
  , processstep1text :: String
  , processexpirywarntext :: String
  , processsendbuttontext :: String
  , processconfirmsendtitle :: String
  , processconfirmsendtext :: String
  , processexpirytext :: String

  -- process specific templates used in doc views
  , processrequiressignguard :: Bool
  , processsignguardwarntext :: String
  , processrestartbuttontext :: String
  , processcancelbuttontext :: String
  , processcancelbyauthormodaltitle :: String
  , processsignatorysignmodaltitle :: String
  , processsignatorysignmodalcontent :: String
  , processsignbuttontext :: String
  , processsignatorycancelmodaltitle :: String
  , processsignatorysignedtext :: String
  , processsignatorycanceledtext :: String
  , processauthorissecretarytext :: String
  , processremindagainbuttontext :: String

  -- process specific doc mail template names
  , processmailcancelbyauthorstandardheader :: String
  , processmailcancelbyauthorcontent :: String
  , processmailclosedcontent :: String
  , processmailrejectcontent :: String
  , processmailinvitationtosigncontent :: String
  , processmailinvitationtosigndefaultheader :: String
  , processmailsignedstandardheader :: String
  , processmailnotsignedstandardheader :: String
  , processmailremindnotsignedcontent :: String

  -- process specific flash message templates
  , processflashmessagecanceled :: String
  , processflashmessagerestarted :: String
  , processflashmessagearchivedone :: String
  , processflashmessagebulkremindssent :: String
  , processflashmessagenobulkremindssent :: String
  , processflashmessagepleasesign :: String

  -- process specific modal templates
  , processmodalsignedviewclosedhasaccount :: String
  , processmodalsignedviewnotclosedhasaccount :: String
  , processmodalsignedviewclosednoaccount :: String
  , processmodalsignedviewnotclosednoaccount :: String
  , processmodalsendconfirmation :: String

  -- process specific seal information
  , processsealincludesmaxtime :: Bool
  , processsealingtext :: String
  , processlasthisentry :: String
  , processinvitationsententry :: String
  , processseenhistentry :: String
  , processsignhistentry :: String

  -- doctexts templates
  , processpendingauthornotsignedinfoheader :: String
  , processpendingauthornotsignedinfotext :: String
  , processpendingauthorinfoheader :: String
  , processpendingauthorinfotext :: String
  , processcancelledinfoheader :: String
  , processcancelledinfotext :: String
  , processsignedinfoheader :: String
  , processsignedinfotext :: String
  , processstatusinfotext :: String
  }

contractProcess :: DocProcessInfo
contractProcess =
  DocProcessInfo {

  -- templates used in lots of different places
    processtitle = "contracttitle"
  , processname = "contractname"

  -- used when uploading
  , processuploadprompttext = "contractuploadprompttext"
  , processuploadname = "contractuploadname"

  -- used in the design view
  , processadvancedview = True
  , processauthorsend = False
  , processvalidationchoiceforbasic = True
  , processexpiryforbasic = True
  , processstep1text = "contractstep1text"
  , processexpirywarntext = "contractexpirywarntext"
  , processsendbuttontext = "contractsendtext"
  , processconfirmsendtitle = "contractconfirmsendtitle"
  , processconfirmsendtext = "contractconfirmsendtext"
  , processexpirytext = "contractexpirytext"

  -- process specific templates used in doc views
  , processrequiressignguard = True
  , processsignguardwarntext = "contractsignguardwarntext"
  , processrestartbuttontext = "contractrestartbuttontext"
  , processcancelbuttontext = "contractcancelbuttontext"
  , processcancelbyauthormodaltitle = "contractcancelbyauthormodaltitle"
  , processsignatorysignmodaltitle = "contractsignatorysignmodaltitle"
  , processsignatorysignmodalcontent = "contractsignatorysignmodalcontent"
  , processsignbuttontext = "contractsignbuttontext"
  , processsignatorycancelmodaltitle = "contractsignatorycancelmodaltitle"
  , processsignatorysignedtext = "signatorysignedcontracttext"
  , processsignatorycanceledtext = "signatorycanceledcontracttext"
  , processauthorissecretarytext = "contractauthorissecretarytext"
  , processremindagainbuttontext = "contractremindagainbuttontext"

  -- process specific doc mail template names
  , processmailcancelbyauthorstandardheader = "mailCancelContractByAuthorStandardHeader"
  , processmailcancelbyauthorcontent = "mailCancelContractByAuthorContent"
  , processmailclosedcontent = "mailContractClosedContent"
  , processmailrejectcontent = "mailRejectContractMailContent"
  , processmailinvitationtosigncontent = "mailInvitationToSignContractContent"
  , processmailinvitationtosigndefaultheader = "mailInvitationToSignContractDefaultHeader"
  , processmailsignedstandardheader = "remindMailSignedContractStandardHeader"
  , processmailnotsignedstandardheader = "remindMailNotSignedContractStandardHeader"
  , processmailremindnotsignedcontent = "remindMailNotSignedContractContent"

  -- process specific flash messages
  , processflashmessagecanceled = "flashMessageContractCanceled"
  , processflashmessagerestarted = "flashMessageContractRestarted"
  , processflashmessagearchivedone = "flashMessageContractArchiveDone"
  , processflashmessagebulkremindssent = "flashMessageBulkContractRemindsSent"
  , processflashmessagenobulkremindssent = "flashMessageNoBulkContractRemindsSent"
  , processflashmessagepleasesign = "flashMessagePleaseSignContract"

  -- process specific modal templates
  , processmodalsignedviewclosedhasaccount = "modalSignedViewClosedHasAccount"
  , processmodalsignedviewnotclosedhasaccount = "modalSignedViewNotClosedHasAccount"
  , processmodalsignedviewclosednoaccount = "modalSignedViewClosedNoAccount"
  , processmodalsignedviewnotclosednoaccount = "modalSignedViewNotClosedNoAccount"
  , processmodalsendconfirmation = "modalSignInviteView"

  -- process specific seal information
  , processsealincludesmaxtime = True
  , processsealingtext = "contractsealingtexts"
  , processlasthisentry = "contractLastHistEntry"
  , processinvitationsententry = "contractInvitationSentEntry"
  , processseenhistentry = "contractSeenHistEntry"
  , processsignhistentry = "contractSignHistEntry"

  -- doctexts templates
  , processpendingauthornotsignedinfoheader = "contractpendingauthornotsignedinfoheader"
  , processpendingauthornotsignedinfotext = "contractpendingauthornotsignedinfotext"
  , processpendingauthorinfoheader = "contractpendingauthorinfoheader"
  , processpendingauthorinfotext = "contractpendingauthorinfotext"
  , processcancelledinfoheader = "contractcancelledinfoheader"
  , processcancelledinfotext = "contractcancelledinfotext"
  , processsignedinfoheader = "contractsignedinfoheader"
  , processsignedinfotext = "contractsignedinfotext"
  , processstatusinfotext = "contractstatusinfotext"
  }

offerProcess :: DocProcessInfo
offerProcess =
  DocProcessInfo {

  -- templates used in lots of different places
    processtitle = "offertitle"
  , processname = "offername"

  -- used when uploading
  , processuploadname = "offeruploadname"
  , processuploadprompttext = "offeruploadprompttext"

  -- used in the design view
  , processadvancedview = False
  , processauthorsend = True
  , processvalidationchoiceforbasic = False
  , processexpiryforbasic = True
  , processstep1text = "offerstep1text"
  , processexpirywarntext = "offerexpirywarntext"
  , processsendbuttontext = "offersendtext"
  , processconfirmsendtitle = "offerconfirmsendtitle"
  , processconfirmsendtext = "offerconfirmsendtext"
  , processexpirytext = "offerexpirytext"

  -- process specific templates used in doc views
  , processrequiressignguard = False
  , processsignguardwarntext = "offersignguardwarntext"
  , processrestartbuttontext = "offerrestartbuttontext"
  , processcancelbuttontext = "offercancelbuttontext"
  , processcancelbyauthormodaltitle = "offercancelbyauthormodaltitle"
  , processsignatorysignmodaltitle = "offersignatorysignmodaltitle"
  , processsignatorysignmodalcontent = "offersignatorysignmodalcontent"
  , processsignbuttontext = "offersignbuttontext"
  , processsignatorycancelmodaltitle = "offersignatorycancelmodaltitle"
  , processsignatorysignedtext = "signatorysignedoffertext"
  , processsignatorycanceledtext = "signatorycanceledoffertext"
  , processauthorissecretarytext = "offerauthorissecretarytext"
  , processremindagainbuttontext = "offerremindagainbuttontext"

  -- process specific doc mail template names
  , processmailcancelbyauthorstandardheader = "mailCancelOfferByAuthorStandardHeader"
  , processmailcancelbyauthorcontent = "mailCancelOfferByAuthorContent"
  , processmailclosedcontent = "mailOfferClosedContent"
  , processmailrejectcontent = "mailRejectOfferMailContent"
  , processmailinvitationtosigncontent = "mailInvitationToSignOfferContent"
  , processmailinvitationtosigndefaultheader = "mailInvitationToSignOfferDefaultHeader"
  , processmailsignedstandardheader = "remindMailSignedOfferStandardHeader"
  , processmailnotsignedstandardheader = "remindMailNotSignedOfferStandardHeader"
  , processmailremindnotsignedcontent = "remindMailNotSignedOfferContent"

  -- process specific flash messages
  , processflashmessagecanceled = "flashMessageOfferCanceled"
  , processflashmessagerestarted = "flashMessageOfferRestarted"
  , processflashmessagearchivedone = "flashMessageOfferArchiveDone"
  , processflashmessagebulkremindssent = "flashMessageBulkOfferRemindsSent"
  , processflashmessagenobulkremindssent = "flashMessageNoBulkOfferRemindsSent"
  , processflashmessagepleasesign = "flashMessagePleaseSignOffer"

  -- process specific modal templates
  , processmodalsignedviewclosedhasaccount = "modalOfferSignedHasAccount"
  , processmodalsignedviewnotclosedhasaccount = "modalOfferSignedHasAccount"
  , processmodalsignedviewclosednoaccount = "modalOfferSignedNoAccount"
  , processmodalsignedviewnotclosednoaccount = "modalOfferSignedNoAccount"
  , processmodalsendconfirmation = "modalOfferCreated"

  -- process specific seal information
  , processsealincludesmaxtime = False
  , processsealingtext = "offersealingtexts"
  , processlasthisentry = "offerLastHistEntry"
  , processinvitationsententry = "offerInvitationSentEntry"
  , processseenhistentry = "offerSeenHistEntry"
  , processsignhistentry = "offerSignHistEntry"

  -- doctexts templates
  , processpendingauthornotsignedinfoheader = "offerpendingauthornotsignedinfoheader"
  , processpendingauthornotsignedinfotext = "offerpendingauthornotsignedinfotext"
  , processpendingauthorinfoheader = "offerpendingauthorinfoheader"
  , processpendingauthorinfotext = "offerpendingauthorinfotext"
  , processcancelledinfoheader = "offercancelledinfoheader"
  , processcancelledinfotext = "offercancelledinfotext"
  , processsignedinfoheader = "offersignedinfoheader"
  , processsignedinfotext = "offersignedinfotext"
  , processstatusinfotext = "offerstatusinfotext"
  }

orderProcess :: DocProcessInfo
orderProcess =
  DocProcessInfo {

  -- templates used in lots of different places
    processtitle = "ordertitle"
  , processname = "ordername"

  -- used when uploading
  , processuploadprompttext = "orderuploadprompttext"
  , processuploadname = "orderuploadname"

  -- used in the design view
  , processadvancedview = True
  , processauthorsend = True
  , processvalidationchoiceforbasic = True
  , processexpiryforbasic = True
  , processstep1text = "orderstep1text"
  , processexpirywarntext = "orderexpirywarntext"
  , processsendbuttontext = "ordersendtext"
  , processconfirmsendtitle = "orderconfirmsendtitle"
  , processconfirmsendtext = "orderconfirmsendtext"
  , processexpirytext = "orderexpirytext"

  -- process specific templates used in doc views
  , processrequiressignguard = False
  , processsignguardwarntext = "ordersignguardwarntext"
  , processrestartbuttontext = "orderrestartbuttontext"
  , processcancelbuttontext = "ordercancelbuttontext"
  , processcancelbyauthormodaltitle = "ordercancelbyauthormodaltitle"
  , processsignatorysignmodaltitle = "ordersignatorysignmodaltitle"
  , processsignatorysignmodalcontent = "ordersignatorysignmodalcontent"
  , processsignbuttontext = "ordersignbuttontext"
  , processsignatorycancelmodaltitle = "ordersignatorycancelmodaltitle"
  , processsignatorysignedtext = "signatorysignedordertext"
  , processsignatorycanceledtext = "signatorycanceledordertext"
  , processauthorissecretarytext = "orderauthorissecretarytext"
  , processremindagainbuttontext = "orderremindagainbuttontext"

  -- process specific doc mail template names
  , processmailcancelbyauthorstandardheader = "mailCancelOrderByAuthorStandardHeader"
  , processmailcancelbyauthorcontent = "mailCancelOrderByAuthorContent"
  , processmailclosedcontent = "mailOrderClosedContent"
  , processmailrejectcontent = "mailRejectOrderMailContent"
  , processmailinvitationtosigncontent = "mailInvitationToSignOrderContent"
  , processmailinvitationtosigndefaultheader = "mailInvitationToSignOrderDefaultHeader"
  , processmailsignedstandardheader = "remindMailSignedOrderStandardHeader"
  , processmailnotsignedstandardheader = "remindMailNotSignedOrderStandardHeader"
  , processmailremindnotsignedcontent = "remindMailNotSignedOrderContent"

  -- process specific flash messages
  , processflashmessagecanceled = "flashMessageOrderCanceled"
  , processflashmessagerestarted = "flashMessageOrderRestarted"
  , processflashmessagearchivedone = "flashMessageOrderArchiveDone"
  , processflashmessagebulkremindssent = "flashMessageBulkOrderRemindsSent"
  , processflashmessagenobulkremindssent = "flashMessageNoBulkOrderRemindsSent"
  , processflashmessagepleasesign = "flashMessagePleaseSignOrder"

  -- process specific modal templates
  , processmodalsignedviewclosedhasaccount = "modalSignedViewOrderClosedHasAccount"
  , processmodalsignedviewnotclosedhasaccount = "modalSignedViewOrderNotClosedHasAccount"
  , processmodalsignedviewclosednoaccount = "modalSignedViewOrderClosedNoAccount"
  , processmodalsignedviewnotclosednoaccount = "modalSignedViewOrderNotClosedNoAccount"
  , processmodalsendconfirmation = "modalOrderCreated"

  -- process specific seal information
  , processsealincludesmaxtime = True
  , processsealingtext = "ordersealingtexts"
  , processlasthisentry = "orderLastHistEntry"
  , processinvitationsententry = "orderInvitationSentEntry"
  , processseenhistentry = "orderSeenHistEntry"
  , processsignhistentry = "orderSignHistEntry"

  -- doctexts templates
  , processpendingauthornotsignedinfoheader = "orderpendingauthornotsignedinfoheader"
  , processpendingauthornotsignedinfotext = "orderpendingauthornotsignedinfotext"
  , processpendingauthorinfoheader = "orderpendingauthorinfoheader"
  , processpendingauthorinfotext = "orderpendingauthorinfotext"
  , processcancelledinfoheader = "ordercancelledinfoheader"
  , processcancelledinfotext = "ordercancelledinfotext"
  , processsignedinfoheader = "ordersignedinfoheader"
  , processsignedinfotext = "ordersignedinfotext"
  , processstatusinfotext = "orderstatusinfotext"
  }

