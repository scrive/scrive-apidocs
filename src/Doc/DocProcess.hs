module Doc.DocProcess (
  DocProcessInfo(..),
  getValueForProcess,
  renderTextForProcess,
  renderTemplateForProcess,
  renderLocalTextForProcess,
  renderLocalTemplateForProcess)
where

import Doc.DocStateData
import Templates.Templates
import User.Locale

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

renderLocalTemplateForProcess :: (HasLocale a, HasProcess a, TemplatesMonad m)
                                 => a
                                 -> (DocProcessInfo -> String)
                                 -> Fields m ()
                                 -> m String
renderLocalTemplateForProcess hasprocess fieldname fields =
  case getValueForProcess hasprocess fieldname of
    Just templatename -> renderLocalTemplate hasprocess templatename fields
    _ -> return ""

renderLocalTextForProcess :: (HasLocale a, HasProcess a, TemplatesMonad m)
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
  getProcess _ = Nothing

instance HasProcess Document where
  getProcess = getProcess . documenttype

data DocProcessInfo =
  DocProcessInfo {

  -- templates used in lots of different places
    processtitle :: String
  , processname :: String
  , processcorename :: String
  -- used when uploading
  , processuploadprompttext :: String
  , processuploadname :: String

  -- used in the design view
  , processbasicavailable :: Bool
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
  , processcancelmodaltitle :: String
  , processcancelmodaltext :: String

  , processrejectbuttontext :: String

  -- Texts for modal that is shown when someone is about to sign (clicked the blue sign button).
  , processsignatorysignmodaltitle :: String
  , processsignatorysignmodalcontentlast :: String           -- Sign & design view. When person is last to sign
  , processsignatorysignmodalcontentnotlast :: String        -- Sign & design view. When person is not last to sign
  , processsignatorysignmodalcontentauthorlast :: String     -- Sign view. When person is author and is last to sign
  , processsignatorysignmodalcontentdesignvieweleg :: String -- Design view, eleg authorization
  , processsignatorysignmodalcontentsignvieweleg   :: String   -- Sign view, eleg authorization
  , processsignatorysignmodalcontentauthoronly     :: String   -- Sign & design view. Only author is signatory.
  , processsignbuttontext :: String
  , processsignbuttontextauthor :: String
  , processsignatorycancelmodaltitle :: String
  , processauthorissecretarytext :: String
  , processremindagainbuttontext :: String

  -- process specific doc mail template names
  , processmailcancelstandardheader :: String
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
  , processflashmessagecanceled :: String
  , processflashmessagerestarted :: String
  , processflashmessagearchivedone :: String
  , processflashmessagebulkremindssent :: String
  , processflashmessagenobulkremindssent :: String

  -- process specific modal templates
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
  , processpendinginfotext :: String
  , processcancelledinfoheader :: String
  , processcancelledinfotext :: String
  , processsignedinfoheader :: String
  , processsignedinfotext :: String
  , processstatusinfotext :: String
  , processauthorsignlastbuttontext :: String

  -- process specific design view titles of parties
  , processauthorname :: String
  , processauthorsignatoryname :: String
  , processsignatoryname :: String
  , processnonsignatoryname :: String
  , processnumberedsignatories :: Bool
  }

contractProcess :: DocProcessInfo
contractProcess =
  DocProcessInfo {

  -- templates used in lots of different places
    processtitle = "contracttitle"
  , processname = "contractname"
  , processcorename = "contractcorename"
  -- used when uploading
  , processuploadprompttext = "contractuploadprompttext"
  , processuploadname = "contractuploadname"

  -- used in the design view
  , processbasicavailable = True
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
  , processcancelmodaltitle = "contractcancelmodaltitle"
  , processcancelmodaltext = "contractcancelmodaltext"
  , processrejectbuttontext = "contractrejectbuttontext"
  , processsignatorysignmodaltitle = "contractsignatorysignmodaltitle"
  , processsignatorysignmodalcontentlast = "contractsignatorysignmodalcontentlast"
  , processsignatorysignmodalcontentnotlast = "contractsignatorysignmodalcontentnotlast"
  , processsignatorysignmodalcontentauthorlast = "contractsignatorysignmodalcontentauthorlast"
  , processsignatorysignmodalcontentdesignvieweleg = "contractsignatorysignmodalcontentdesignvieweleg"
  , processsignatorysignmodalcontentsignvieweleg = "contractsignatorysignmodalcontentsignvieweleg"
  , processsignatorysignmodalcontentauthoronly = "contractsignatorysignmodalcontentauthoronly"
  , processsignbuttontext = "contractsignbuttontext"
  , processsignbuttontextauthor = "contractsignbuttontextauthor"
  , processsignatorycancelmodaltitle = "contractsignatorycancelmodaltitle"
  , processauthorissecretarytext = "contractauthorissecretarytext"
  , processremindagainbuttontext = "contractremindagainbuttontext"

  -- process specific doc mail template names
  , processmailcancelstandardheader = "mailCancelContractStandardHeader"
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
  , processflashmessagecanceled = "flashMessageContractCanceled"
  , processflashmessagerestarted = "flashMessageContractRestarted"
  , processflashmessagearchivedone = "flashMessageContractArchiveDone"
  , processflashmessagebulkremindssent = "flashMessageBulkContractRemindsSent"
  , processflashmessagenobulkremindssent = "flashMessageNoBulkContractRemindsSent"

  -- process specific modal templates
  , processmodalsendconfirmation = "modalContractCreated"

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
  , processpendinginfotext = "contractpendinginfotext"
  , processcancelledinfoheader = "contractcancelledinfoheader"
  , processcancelledinfotext = "contractcancelledinfotext"
  , processsignedinfoheader = "contractsignedinfoheader"
  , processsignedinfotext = "contractsignedinfotext"
  , processstatusinfotext = "contractstatusinfotext"
  , processauthorsignlastbuttontext = "contractauthorsignlastbuttontext"

  , processauthorname = "contractauthorname"
  , processauthorsignatoryname = "contractauthorsignatoryname"
  , processsignatoryname = "contractsignatoryname"
  , processnonsignatoryname = "contractnonsignatoryname"
  , processnumberedsignatories = True
  }

offerProcess :: DocProcessInfo
offerProcess =
  DocProcessInfo {

  -- templates used in lots of different places
    processtitle = "offertitle"
  , processname = "offername"
  , processcorename = "offercorename"

  -- used when uploading
  , processuploadname = "offeruploadname"
  , processuploadprompttext = "offeruploadprompttext"

  -- used in the design view
  , processbasicavailable = True
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
  , processcancelmodaltitle = "offercancelmodaltitle"
  , processcancelmodaltext = "offercancelmodaltext"

  , processrejectbuttontext = "offerrejectbuttontext"
  , processsignatorysignmodaltitle = "offersignatorysignmodaltitle"
  , processsignatorysignmodalcontentlast = "offersignatorysignmodalcontentlast"
  , processsignatorysignmodalcontentnotlast = "offersignatorysignmodalcontentnotlast"
  , processsignatorysignmodalcontentauthorlast = "offersignatorysignmodalcontentauthorlast"
  , processsignatorysignmodalcontentdesignvieweleg = "offersignatorysignmodalcontentdesignvieweleg"
  , processsignatorysignmodalcontentsignvieweleg = "offersignatorysignmodalcontentsignvieweleg"
  , processsignatorysignmodalcontentauthoronly = "offersignatorysignmodalcontentauthoronly"
  , processsignbuttontext = "offersignbuttontext"
  , processsignbuttontextauthor = "offersignbuttontextauthor"
  , processsignatorycancelmodaltitle = "offersignatorycancelmodaltitle"
  , processauthorissecretarytext = "offerauthorissecretarytext"
  , processremindagainbuttontext = "offerremindagainbuttontext"

  -- process specific doc mail template names
  , processmailcancelstandardheader = "mailCancelOfferStandardHeader"
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
  , processflashmessagecanceled = "flashMessageOfferCanceled"
  , processflashmessagerestarted = "flashMessageOfferRestarted"
  , processflashmessagearchivedone = "flashMessageOfferArchiveDone"
  , processflashmessagebulkremindssent = "flashMessageBulkOfferRemindsSent"
  , processflashmessagenobulkremindssent = "flashMessageNoBulkOfferRemindsSent"

  -- process specific modal templates
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
  , processpendinginfotext = "offerpendinginfotext"
  , processcancelledinfoheader = "offercancelledinfoheader"
  , processcancelledinfotext = "offercancelledinfotext"
  , processsignedinfoheader = "offersignedinfoheader"
  , processsignedinfotext = "offersignedinfotext"
  , processstatusinfotext = "offerstatusinfotext"
  , processauthorsignlastbuttontext = "offerauthorsignlastbuttontext"

  , processauthorname = "offerauthorname"
  , processauthorsignatoryname = "offerauthorsignatoryname"
  , processsignatoryname = "offersignatoryname"
  , processnonsignatoryname = "offernonsignatoryname"
  , processnumberedsignatories = True

  }

orderProcess :: DocProcessInfo
orderProcess =
  DocProcessInfo {

  -- templates used in lots of different places
    processtitle = "ordertitle"
  , processname = "ordername"
  , processcorename = "ordercorename"

  -- used when uploading
  , processuploadprompttext = "orderuploadprompttext"
  , processuploadname = "orderuploadname"

  -- used in the design view
  , processbasicavailable = False
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
  , processcancelmodaltitle = "ordercancelmodaltitle"
  , processcancelmodaltext = "ordercancelmodaltext"

  , processrejectbuttontext = "orderrejectbuttontext"
  , processsignatorysignmodaltitle = "ordersignatorysignmodaltitle"
  , processsignatorysignmodalcontentlast = "ordersignatorysignmodalcontentlast"
  , processsignatorysignmodalcontentnotlast = "ordersignatorysignmodalcontentnotlast"
  , processsignatorysignmodalcontentauthorlast = "ordersignatorysignmodalcontentauthorlast"
  , processsignatorysignmodalcontentdesignvieweleg = "ordersignatorysignmodalcontentdesignvieweleg"
  , processsignatorysignmodalcontentsignvieweleg = "ordersignatorysignmodalcontentsignvieweleg"
  , processsignatorysignmodalcontentauthoronly = "ordersignatorysignmodalcontentauthoronly"
  , processsignbuttontext = "ordersignbuttontext"
  , processsignbuttontextauthor = "ordersignbuttontextauthor"
  , processsignatorycancelmodaltitle = "ordersignatorycancelmodaltitle"
  , processauthorissecretarytext = "orderauthorissecretarytext"
  , processremindagainbuttontext = "orderremindagainbuttontext"

  -- process specific doc mail template names
  , processmailcancelstandardheader = "mailCancelOrderStandardHeader"
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
  , processflashmessagecanceled = "flashMessageOrderCanceled"
  , processflashmessagerestarted = "flashMessageOrderRestarted"
  , processflashmessagearchivedone = "flashMessageOrderArchiveDone"
  , processflashmessagebulkremindssent = "flashMessageBulkOrderRemindsSent"
  , processflashmessagenobulkremindssent = "flashMessageNoBulkOrderRemindsSent"

  -- process specific modal templates
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
  , processpendinginfotext = "orderpendinginfotext"
  , processcancelledinfoheader = "ordercancelledinfoheader"
  , processcancelledinfotext = "ordercancelledinfotext"
  , processsignedinfoheader = "ordersignedinfoheader"
  , processsignedinfotext = "ordersignedinfotext"
  , processstatusinfotext = "orderstatusinfotext"
  , processauthorsignlastbuttontext = "orderauthorsignlastbuttontext"

  , processauthorname = "orderauthorname"
  , processauthorsignatoryname = "orderauthorsignatoryname"
  , processsignatoryname = "ordersignatoryname"
  , processnonsignatoryname = "ordernonsignatoryname"
  , processnumberedsignatories = True

  }

