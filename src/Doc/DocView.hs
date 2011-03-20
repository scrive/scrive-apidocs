{-# OPTIONS_GHC -Wall #-}

module Doc.DocView (
    emptyDetails
  , showFilesImages2
  , pageDocumentForAuthor
  , pageDocumentForViewer
  , pageDocumentForSignatory
  , docSortSearchPage
  , pageContractsList
  , pageTemplatesList
  , landpageSignInviteView
  , landpageSendInviteView
  , landpageSignedView
  , landpageLoginForSaveView
  , flashRemindMailSent
  , flashMessageCanceled
  , flashDocumentRestarted
  , flashDocumentDraftSaved
  , flashAuthorSigned
  , landpageRejectedView
  , defaultInviteMessage
  , mailDocumentRemind
  , mailDocumentRejected
  , mailDocumentAwaitingForAuthor
  , mailCancelDocumentByAuthorContent
  , mailCancelDocumentByAuthor
  , mailInvitationToSign
  , mailInvitationToSend
  , mailDocumentClosedForSignatories
  , mailDocumentClosedForAuthor
  , isNotLinkForUserID
  , signatoryDetailsFromUser
  , uploadPage
  , templatesForAjax
  ) where

import Control.Applicative ((<$>))
import Data.Data
import Data.List (find)
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Ord
import Doc.DocState
import Doc.DocViewMail
import Doc.DocViewUtil
import Kontra
import KontraLink
import Mails.MailsUtil
import MinutesTime
import Misc
import Templates.Templates
import Templates.TemplatesUtils
import User.UserView (prettyName)
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.List (isInfixOf,sortBy)
import Data.Monoid
import ListUtil

landpageSignInviteView :: KontrakcjaTemplates -> Document -> IO String
landpageSignInviteView templates document = do
  partylist <- renderListTemplate templates . map (BS.toString . personname') $ partyListButAuthor document
  renderTemplate templates "landpageSignInviteView" $ do
    field "partyListButAuthor" partylist
    field "documenttitle" . BS.toString $ documenttitle document


landpageSendInviteView :: KontrakcjaTemplates -> Document -> IO String
landpageSendInviteView templates document = do
  partylist <- renderListTemplate templates . map (BS.toString . personname') $ partyListButAuthor document
  renderTemplate templates  "landpageSendInviteView" $ do
    field "partyListButAuthor" partylist
    field "documenttitle" . BS.toString $ documenttitle document


willCreateAccountForYou :: KontrakcjaTemplates -> Document -> SignatoryLink -> Bool -> IO String
willCreateAccountForYou templates document siglink hasAccount =
  if (hasAccount)
     then
       renderTemplate templates "willCreateAccountForYouHasAccount" $ do
         field "email" . signatoryemail $ signatorydetails siglink
     else
       renderTemplate templates "willCreateAccountForYouNoAccount" $ do
         field "documentid" $ show $ unDocumentID $ documentid document
         field "signatorylinkid" $ unSignatoryLinkID $ signatorylinkid siglink


landpageRejectedView :: KontrakcjaTemplates -> Document -> IO String
landpageRejectedView templates document = do
  partylist <-renderListTemplate templates . map (BS.toString . personname') $ partyList document
  renderTemplate templates "landpageRejectedView" $ do
    field "partyList" partylist
    field "documenttitle" . BS.toString $ documenttitle document


landpageSignedView :: KontrakcjaTemplates -> Document -> SignatoryLink -> Bool -> IO String
landpageSignedView templates document@Document{documenttitle, documentstatus} signatorylink hasaccount =
  if documentstatus == Closed
     then
       renderTemplate templates "landpageSignedViewClosed" $ do
         field "partyListString" . renderListTemplate templates . map (BS.toString . personname') $ partyList document
         field "documenttitle" $ BS.toString $ documenttitle
         field "willCreateAccountForYou" $ willCreateAccountForYou templates document signatorylink hasaccount
     else
       renderTemplate templates "landpageSignedViewNotClosed" $ do
         field "partyUnsignedListString" . renderListTemplate templates . map (BS.toString . personname') $ partyUnsignedList document
         field "documenttitle" . BS.toString $ documenttitle
         field "willCreateAccountForYou" $ willCreateAccountForYou templates document signatorylink hasaccount 


landpageLoginForSaveView :: KontrakcjaTemplates -> IO String
landpageLoginForSaveView templates =
  renderTemplate templates "landpageLoginForSaveView" ()


flashDocumentDraftSaved :: KontrakcjaTemplates -> IO FlashMessage
flashDocumentDraftSaved templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashDocumentDraftSaved" ()


flashDocumentRestarted :: KontrakcjaTemplates -> IO FlashMessage
flashDocumentRestarted templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashDocumentRestarted" ()

flashRemindMailSent :: KontrakcjaTemplates -> SignatoryLink -> IO FlashMessage
flashRemindMailSent templates signlink@SignatoryLink{maybesigninfo} =
  toFlashMsg OperationDone <$> (renderTemplate templates (template_name maybesigninfo) $ do
    field "personname" . BS.toString $ personname signlink)
  where
    template_name =
      maybe "flashRemindMailSentNotSigned"
      (const "flashRemindMailSentSigned")


flashMessageCanceled :: KontrakcjaTemplates -> IO FlashMessage
flashMessageCanceled templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashMessageCanceled" ()

flashAuthorSigned :: KontrakcjaTemplates -> IO FlashMessage
flashAuthorSigned templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashAuthorSigned" ()
  
-- All doc view
singlnkFields :: SignatoryLink -> Fields
singlnkFields sl = do
  field "id" $ show $ signatorylinkid sl
  field "name" $ BS.toString $ personname sl
  field "email" $  ""
  field "company" $ ""
  

documentStatusClass ::Document -> String
documentStatusClass doc = 
    case documentstatus doc of
         Preparation    -> "draft"
         Closed         -> "signed"
         Canceled       -> "cancelled"
         Timedout       -> "cancelled"
         Rejected       -> "cancelled"
         Pending        -> if anyInvitationUndelivered doc
                               then "cancelled"
                               else
                                if all (isJust . maybeseeninfo) $ documentsignatorylinks doc
                                 then "opened"
                                 else "sent"
         AwaitingAuthor -> if anyInvitationUndelivered doc
                                then "cancelled"
                                else
                                  if all (isJust . maybeseeninfo) $ documentsignatorylinks doc
                                    then "opened"
                                    else "sent"
         _              -> "cancelled"

documentBasicViewFields :: MinutesTime -> User -> Document -> Fields
documentBasicViewFields crtime user doc = do
    documentInfoFields doc
    field "status" $ documentStatusClass doc
    field "signatories" $ map singlnkFields $ documentsignatorylinks doc
    field "anyinvitationundelivered" $ anyInvitationUndelivered doc
    field "doclink"  $ if (unAuthor $ documentauthor doc) == userid user || null signatorylinklist
                        then show . LinkIssueDoc $ documentid doc
                        else show $ LinkSignDoc doc (head signatorylinklist)
    field "davelink" $ if isSuperUser (Just user)
                        then Just $ "/dave/document/" ++ (show $ documentid doc)
                        else Nothing
    field "timeoutdate" $ fromTimeout show
    field "timeoutdaysleft" $ fromTimeout $ show . (dateDiffInDays crtime)
    field "mtime" $ showDateAbbrev crtime (documentmtime doc)
    field "isauthor" $ isAuthor doc user
  
  where
    signatorylinklist =
      filter (isMatchingSignatoryLink user) $ documentsignatorylinks doc  
    
    fromTimeout f =
      case (documenttimeouttime doc, documentstatus doc) of
           (Just (TimeoutTime x), Pending) -> Just $ f x
           _                               -> Nothing



-- Searching, sorting and paging
docSortSearchPage :: ListParams -> [Document] -> PagedList Document
docSortSearchPage  = listSortSearchPage docSortFunc docSearchFunc docsPageSize

docSearchFunc::SearchingFunction Document
docSearchFunc s doc =  nameMatch doc || signMatch doc
    where
    match m = isInfixOf (map toUpper s) (map toUpper m)
    nameMatch = match . BS.toString . documenttitle
    signMatch d = any match $ map (BS.toString . personname) (documentsignatorylinks d)
    
   
docSortFunc:: SortingFunction Document
docSortFunc "status" = viewComparing documentStatusClass
docSortFunc "statusREV" = viewComparingRev documentStatusClass
docSortFunc "title" = viewComparing documenttitle
docSortFunc "titleREV" = viewComparingRev documenttitle
docSortFunc "time" = viewComparing documentmtime
docSortFunc "timeREV" = viewComparingRev documentmtime
docSortFunc "partner" = viewComparing $ BS.concat . (map personname) . documentsignatorylinks 
docSortFunc "partnerREV" = viewComparingRev $ BS.concat . (map personname) . documentsignatorylinks 
docSortFunc _ = const $ const EQ


docsPageSize :: Int
docsPageSize = 20
 

--

pageContractsList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String
pageContractsList templates ctime user documents =
  renderTemplate templates "pageContractsList" $ do
    field "documents" $ markParity $ map (documentBasicViewFields ctime user) $ list documents
    pagedListFields documents
    field "currentlink" $ show $ LinkContracts $ params documents
        

pageTemplatesList :: KontrakcjaTemplates -> MinutesTime -> User -> PagedList Document -> IO String
pageTemplatesList templates ctime user documents =
 renderTemplate templates "pageTemplatesList" $ do
    field "documents" $ markParity $ map (documentBasicViewFields ctime user) $ list documents
    pagedListFields documents
    field "currentlink" $ show $ LinkTemplates $ params documents
    
          
showFileImages :: KontrakcjaTemplates -> File -> JpegPages -> IO String
showFileImages templates _ JpegPagesPending =
  renderTemplate templates  "showFileImagesPending" ()

showFileImages templates _ (JpegPagesError normalizelog) =
  renderTemplate templates "showFileImagesError" $ do
    field "normalizelog" $ BS.toString normalizelog

showFileImages templates File{fileid} (JpegPages jpgpages) =
  renderTemplate templates "showFileImagesReady" $ do
    field "fileid" $ show fileid
    field "images" . map page $ zip [1,2..] jpgpages
  where
    page :: (Int,(a,Int,Int)) -> Fields
    page (x,(_,w,h)) = do
      field "number" x
      field "width" w
      field "height" h


showFilesImages2 :: KontrakcjaTemplates -> [(File, JpegPages)] -> IO String
showFilesImages2 templates files = do
  filesPages <- sequence $ map (uncurry (showFileImages templates)) files
  renderTemplate templates  "spanNoEscape" $ field "it" (concat filesPages)


{-

   Document is invalid
   Fel filformat
   Vi beklagar, fel filformat

   mp3 -- we cannot do anything with this document
-}

emptyDetails :: SignatoryDetails
emptyDetails =
  SignatoryDetails {
      signatoryfstname = BS.empty
    , signatorysndname = BS.empty
    , signatorycompany = BS.empty
    , signatorynumber = BS.empty
    , signatoryemail = BS.empty
    , signatoryfstnameplacements = []
    , signatorysndnameplacements = []
    , signatorycompanyplacements = []
    , signatorynumberplacements = []
    , signatoryemailplacements = []
    , signatoryotherfields = []
  }

{- |
   link does not belong to user with uid
 -}
isNotLinkForUserID :: UserID
                   -> SignatoryLink
                   -> Bool
isNotLinkForUserID uid link =
    hasNoUserID || notSameUserID
        where hasNoUserID = isNothing $ maybesignatory link
              notSameUserID = uid /= linkuid
              linkuid = unSignatory $ fromJust $ maybesignatory link

pageDocumentForAuthor :: Context 
             -> Document 
             -> User
             -> (Maybe DesignStep)
             -> IO String
pageDocumentForAuthor ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentstatus
    , documentdaystosign
    , documentinvitetext
    , documentallowedidtypes
  }
  author 
  step =
   let
       templates = ctxtemplates ctx
       authorid = userid author
       authorhaslink = not $ null $ filter (not . isNotLinkForUserID authorid) documentsignatorylinks
       documentdaystosignboxvalue = maybe 7 id documentdaystosign
       documentauthordetails =
         (signatoryDetailsFromUser author) {
             signatoryemailplacements = authoremailplacements document
           , signatoryfstnameplacements = authorfstnameplacements document
           , signatorysndnameplacements = authorsndnameplacements document
           , signatorycompanyplacements = authorcompanyplacements document
           , signatorynumberplacements = authornumberplacements document
           , signatoryotherfields = authorotherfields document
         }
       doc_author_otherfields fields = sequence .
         map (\(fd, i) ->
           renderTemplate templates "customfield" $ do
             field "otherFieldValue" $ fieldvalue fd
             field "otherFieldName"  $ fieldlabel fd
             field "otherFieldID"    $ "field" ++ show i
             field "otherFieldOwner" "author")
             $ zip fields ([1..]::[Int])
   in do
     validationinput <- if isSuperUser $ Just author
                        then renderTemplate (ctxtemplates ctx) "validationdropdown" ()
                        else renderTemplate (ctxtemplates ctx) "emailhidden" ()
     renderTemplate (ctxtemplates ctx) "pageDocumentForAuthorContent" $ do
       field "authorOtherFields" $ doc_author_otherfields $ signatoryotherfields documentauthordetails
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "authorhaslink" $ authorhaslink
       field "documentinvitetext" $ documentinvitetext
       field "invitationMailContent" $  mailInvitationToSignContent templates False ctx document author Nothing
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue
       field "anyinvitationundelivered" $ anyInvitationUndelivered document
       field "undelivered" $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document
       field "signatories" $ fmap concat $ sequence $ map (showSignatoryLinkForSign ctx document author) documentsignatorylinks                    
       field "canberestarted" $ documentstatus `elem` [Canceled, Timedout, Rejected]
       field "restartForm" $ renderActionButton templates (LinkRestart documentid) "restartButtonName"
       field "cancelMailContent" $ mailCancelDocumentByAuthorContent templates False Nothing ctx document author
       field "linkcancel" $ show $ LinkCancel document
       field "emailelegitimation" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "emailonly" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isNothing $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "elegitimationonly" $ (isNothing $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "docstate" (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks)
       field "linkissuedocpdf" $ show (LinkIssueDocPDF document)
       field "documentinfotext" $ documentInfoText templates document (find (isMatchingSignatoryLink author) documentsignatorylinks) author
       documentAuthorInfo author
       field "validationinput" validationinput
       documentInfoFields document
       designViewFields step

renderActionButton :: KontrakcjaTemplates -> KontraLink -> String -> IO String
renderActionButton templates action button = do
  buttonname <- renderTemplate templates button ()
  renderTemplate templates "actionButton" [("action", show action), ("buttonname", buttonname)]
  
{- |
   Show the document for Viewers (friends of author or signatory).
   Show no buttons or other controls
 -}                                                                                                          

pageDocumentForViewer :: Context -> Document -> User -> IO String
pageDocumentForViewer ctx
  document@Document {
      documentsignatorylinks
    , documentid
    , documentstatus
    , documentdaystosign
    , documentinvitetext
    , documentallowedidtypes
  }
  author =
    let
        authorid = userid author
        -- the author gets his own space when he's editing
        authorhaslink = not $ null $ filter (not . isNotLinkForUserID authorid) documentsignatorylinks
        documentdaystosignboxvalue = maybe 7 id documentdaystosign
        documentauthordetails =
          (signatoryDetailsFromUser author) {
            signatoryemailplacements = authoremailplacements document
            , signatoryfstnameplacements = authorfstnameplacements document
            , signatorysndnameplacements = authorsndnameplacements document
            , signatorycompanyplacements = authorcompanyplacements document
            , signatorynumberplacements = authornumberplacements document
            , signatoryotherfields = authorotherfields document
          }
   in do
     helpers <- renderTemplate (ctxtemplates ctx) "pageDocumentForViewerHelpers" [("documentid", show documentid)]
     signatories <- fmap concat $ sequence $ map (showSignatoryLinkForSign ctx document author) documentsignatorylinks
     invitationMailContent <- mailInvitationToSignContent (ctxtemplates ctx) False ctx document author Nothing
     restartForm <- renderActionButton  (ctxtemplates ctx) (LinkRestart documentid) "restartButtonName"
     cancelMailContent <- mailCancelDocumentByAuthorContent (ctxtemplates ctx) False Nothing ctx document author
     documentinfotext <- documentInfoText (ctxtemplates ctx) document Nothing author
     renderTemplate (ctxtemplates ctx) "pageDocumentForViewerContent" $  do
       field "linkissuedoc" $ show $ LinkIssueDoc documentid
       field "authorhaslink" $ authorhaslink
       field "documentinvitetext" $ documentinvitetext
       field "invitationMailContent" $ invitationMailContent
       field "documentdaystosignboxvalue" $ documentdaystosignboxvalue
       field "anyinvitationundelivered" $ anyInvitationUndelivered document
       field "undelivered" $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document
       field "signatories" signatories
       field "canberestarted" $ documentstatus `elem` [Canceled, Timedout, Rejected]
       field "restartForm" $ restartForm
       field "cancelMailContent" $ cancelMailContent
       field "linkcancel" $ show $ LinkCancel document
       field "emailelegitimation" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "emailonly" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isNothing $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "elegitimationonly" $ (isNothing $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)
       field "helpers" helpers
       field "docstate" (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks)
       field "linkissuedocpdf" $ show (LinkIssueDocPDF document)
       field "documentinfotext" $ documentinfotext
       documentInfoFields document 
       documentAuthorInfo author


pageDocumentForSignatory :: KontraLink 
                    -> Document 
                    -> Context
                    -> SignatoryLink
                    -> User
                    -> IO String 
pageDocumentForSignatory action document ctx invitedlink author =
  let
      localscripts =
           "var docstate = "
        ++ (buildJS documentauthordetails $ map signatorydetails (documentsignatorylinks document))
        ++ "; docstate['useremail'] = '"
        ++ (BS.toString $ signatoryemail $ signatorydetails invitedlink)
        ++ "';"
      magichash = signatorymagichash invitedlink
      allbutinvited = filter (/= invitedlink) (documentsignatorylinks document)
      documentauthordetails = signatoryDetailsFromUser author
      allowedtypes = documentallowedidtypes document
      requiresEleg = isJust $ find (== ELegitimationIdentification) allowedtypes
  in do
    renderTemplate (ctxtemplates ctx) "pageDocumentForSignContent" $ do
      field "helpers" $ 
          renderTemplate (ctxtemplates ctx) "pageDocumentForSignHelpers" $ do
              field "documentid" . show $ documentid document
              field "localscripts" localscripts
      field "signatories" $ fmap concat $ sequence $ map (showSignatoryLinkForSign ctx document author) (invitedlink : allbutinvited)
      field "rejectMessage" $  mailRejectMailContent (ctxtemplates ctx) Nothing ctx (prettyName author) document invitedlink
      field "partyUnsigned" $ renderListTemplate (ctxtemplates ctx) $  map (BS.toString . personname') $ partyUnsignedMeAndList magichash document
      field "action" $ show action
      field "linkissuedocpdf" $ show (LinkIssueDocPDF document)
      field "documentinfotext" $  documentInfoText (ctxtemplates ctx) document (Just invitedlink) author
      field "requireseleg" requiresEleg
      documentInfoFields document
      documentAuthorInfo author
      signedByMeFields document (Just invitedlink)


--- Display of signatory                                                             
showSignatoryLinkForSign :: Context -> Document -> User -> SignatoryLink -> IO String
showSignatoryLinkForSign
  ctx@Context {
      ctxmaybeuser = muser
    , ctxtemplates
  }
  document
  author
  siglnk@SignatoryLink {
    signatorylinkid
    , maybesigninfo
    , maybeseeninfo
    , invitationdeliverystatus
    , signatorydetails =
      SignatoryDetails {
        signatorynumber
        , signatorycompany
        , signatoryemail
        , signatoryotherfields
      }
  } =
  let
      isCurrentUserAuthor = maybe False (isAuthor document) muser
      isCurrentSignatorAuthor = (fmap (unEmail . useremail . userinfo) muser) == (Just signatoryemail)      
      wasSigned =  isJust maybesigninfo && (not $ isCurrentSignatorAuthor && (documentstatus document == AwaitingAuthor))
      wasSeen = isJust maybeseeninfo
      isTimedout = documentstatus document == Timedout
      isCanceled = documentstatus document == Canceled
      isRejected = documentstatus document == Rejected
      isClosed = documentstatus document == Closed

      -- isWithDrawn = documentstatus document == Withdrawn
      dontShowAnyReminder = isTimedout || isCanceled || isRejected
      dialogHeight =   if (wasSigned) then "400" else "600"
      status = caseOf [
          (invitationdeliverystatus == Undelivered, "/theme/images/status_rejected.png")
        --, (isWithDrawn,"/theme/images/status_rejected.png")
        , (isCanceled, "/theme/images/status_rejected.png")
        , (isRejected, "/theme/images/status_rejected.png")
        , (isTimedout, "/theme/images/status_timeout.png")
        , (wasSigned,  "/theme/images/status_signed.png")
        , (wasSeen,    "/theme/images/status_viewed.png")
        ] "/theme/images/status_pending.png"
      
      signatoryFields =
        if documentstatus document == Closed
           then signatoryotherfields
           else []
  in do
    message <- caseOf [
        (wasSigned, renderTemplate ctxtemplates "signatoryMessageSigned" [("date", showDateOnly $ signtime $ fromJust maybesigninfo)])
      , (isTimedout, renderTemplate ctxtemplates "signatoryMessageTimedout" ())
      , (isCanceled || isRejected, return "" )
      , (wasSeen, renderTemplate ctxtemplates "signatoryMessageSeen" [("date", showDateOnly $ signtime $ fromJust maybeseeninfo)])
      ] $ renderTemplate ctxtemplates "signatoryMessageNotSigned" ()
    reminderText <-
      if (isClosed)
         then renderTemplate ctxtemplates "reminderTextSigned" () 
         else
           if (not wasSigned)
              then renderTemplate ctxtemplates "reminderTextNotSigned" ()
              else return ""
    reminderSenderText <-
      if (isClosed)
         then renderTemplate ctxtemplates "reminderSenderTextSigned" ()
         else
           if (not wasSigned)
              then renderTemplate ctxtemplates "reminderSenderTextNotSigned" ()
              else return ""
    reminderEditorText <- renderTemplate ctxtemplates "reminderEditorText" ()                         
    reminderDialogTitle <- return reminderText   
    reminderMessage <- mailDocumentRemindContent ctxtemplates Nothing ctx document siglnk author
    reminderForm <- whenMaybe (isCurrentUserAuthor && (not isCurrentSignatorAuthor) && (not dontShowAnyReminder) && (invitationdeliverystatus /= Undelivered)) $
      renderTemplate ctxtemplates "reminderForm" $
        (setAttribute "reminderDialogTitle" $  reminderDialogTitle) .
        (setAttribute "signatorylinkid" $  show signatorylinkid ) .
        (setAttribute "reminderText" $  reminderDialogTitle) .
        (setAttribute "reminderMessage" $ reminderMessage ) .
        (setAttribute "reminderEditorText" $ reminderEditorText) .
        (setAttribute "reminderSenderText" $ reminderSenderText ) .
        (setAttribute "dialogHeight" $ dialogHeight ) .
        (setAttribute "linkremind" $ show (LinkRemind document siglnk)) 
    changeEmailAddress <- whenMaybe (isCurrentUserAuthor && (invitationdeliverystatus == Undelivered) && (not dontShowAnyReminder)) $
      renderTemplate ctxtemplates "changeEmailAddress" $
        (setAttribute "linkchangeemail" $  show $ LinkChangeSignatoryEmail (documentid document) signatorylinkid) .
        (setAttribute "signatoryemail" $  BS.toString signatoryemail)
    renderTemplate ctxtemplates "showSignatoryLinkForSign" $  do
      field "mainclass" $ if isCurrentSignatorAuthor then "author" else "signatory"
      field "status" status
      field "signatoryfstname" $ packToMString $ signatoryfstname $ signatorydetails siglnk
      field "signatorysndname" $ packToMString $ signatorysndname $ signatorydetails siglnk
      field "signatorycompany" $ packToMString signatorycompany
      field "signatorynumber" $ packToMString signatorynumber
      field "signatoryemail" $ packToMString signatoryemail
      field "fields" $ for signatoryFields $ \sof -> do
        field "fieldlabel" $ fieldlabel sof
        field "fieldvalue" $ fieldvalue sof
      field "message" message
      field "reminderForm" reminderForm
      field "changeEmailAddress" changeEmailAddress


packToMString :: BS.ByteString -> Maybe String
packToMString x =
  if BS.null x
     then Nothing
     else Just $ BS.toString x


-- Helper to get document after signing info text
documentInfoText :: KontrakcjaTemplates -> Document -> Maybe SignatoryLink -> User -> IO String
documentInfoText templates document siglnk author =
  renderTemplate templates "documentInfoText" $ do
    documentInfoFields document 
    documentAuthorInfo author
    signedByMeFields document siglnk

-- | Basic info about document , name, id ,author
documentInfoFields :: Document -> Fields
documentInfoFields  document  = do
  field "documenttitle" $ BS.toString $ documenttitle document
  field "title" $ BS.toString $ documenttitle document
  field "name" $ BS.toString $ documenttitle document
  field "id" $ show $ documentid document
  field "documentid" $ show $ documentid document
  field "timetosignset" $  isJust $ documentdaystosign document
  field "template" $  documenttype document == Template
  field "contract" $  documenttype document == Contract
  documentStatusFields document

documentAuthorInfo :: User -> Fields
documentAuthorInfo author =  do
  field "authorfstname" $ nothingIfEmpty $ userfstname $ userinfo author
  field "authorsndname" $ nothingIfEmpty $ usersndname $ userinfo author
  field "authorcompany" $ nothingIfEmpty $ usercompanyname $ userinfo author
  field "authoremail"  $ nothingIfEmpty $ unEmail $ useremail $ userinfo author
  field "authorcompanynumber" $ nothingIfEmpty $ usercompanynumber $ userinfo author
  
-- | Fields indication what is a document status 
documentStatusFields :: Document -> Fields    
documentStatusFields document = do
  field "preparation" $ documentstatus document == Preparation
  field "pending" $ documentstatus document == Pending
  field "cancel" $ documentstatus document ==  Canceled
  field "timedout" $ documentstatus document == Timedout
  field "rejected" $ documentstatus document == Rejected
  field "signed" $ documentstatus document == Closed
  field "awaitingauthor" $ documentstatus document == AwaitingAuthor

-- | Info about what is my position on a document
signedByMeFields :: Document -> Maybe SignatoryLink -> Fields
signedByMeFields document siglnk = do
  field "notsignedbyme" $ (isJust siglnk) && (isNothing $ maybesigninfo $ fromJust siglnk)
  field "signedbyme" $ (isJust siglnk) && (isJust $ maybesigninfo $ fromJust siglnk)
  field "iamauthor" $ isAuthor document siglnk

designViewFields:: (Maybe DesignStep) -> Fields
designViewFields step = do
    case step of 
        (Just (DesignStep3 _)) -> field "step3" True
        (Just (DesignStep2 _)) -> field "step2" True
        (Just (DesignStep1)) -> field "step1" True
        _ -> field "step2" True
        

uploadPage :: KontrakcjaTemplates -> ListParams -> Bool -> IO String
uploadPage templates params showTemplates = renderTemplate templates "uploadPage" $ do
    field "templateslink" $ show $ LinkAjaxTemplates params
    field "showTemplates" showTemplates
    
    
  

templatesForAjax::KontrakcjaTemplates ->  MinutesTime -> User -> PagedList Document -> IO String
templatesForAjax templates ctime user doctemplates = 
    renderTemplate templates "templatesForAjax" $ do
        field "documents" $ markParity $ map (documentBasicViewFields ctime user) (list doctemplates)
        field "currentlink" $ show $ LinkNew (params doctemplates)    
        pagedListFields doctemplates
    
-- We keep this javascript code generation for now
jsArray :: [[Char]] -> [Char]
jsArray xs = "[" ++ (joinWith ", " xs) ++ "]"


buildDefJS :: FieldDefinition -> Int -> [Char]
buildDefJS FieldDefinition {
    fieldlabel
  , fieldvalue
  , fieldplacements
  } i =
     "{ label: "
  ++ jsStringFromBS fieldlabel -- show because we need quotes
  ++ ", value: "
  ++ jsStringFromBS fieldvalue
  ++ ", id: 'field" ++ show i ++ "'"
  ++ ", placements: " ++ (jsArray (map buildPlacementJS fieldplacements))
  ++ " }"


buildPlacementJS :: FieldPlacement -> [Char]
buildPlacementJS FieldPlacement {
    placementx
  , placementy
  , placementpage
  , placementpagewidth
  , placementpageheight
  } =
     "{ x: "
  ++ show placementx
  ++ ", y: " ++ show placementy
  ++ ", page: " ++ show placementpage
  ++ ", h: " ++ show placementpageheight
  ++ ", w: " ++ show placementpagewidth
  ++ " }"


buildSigJS :: SignatoryDetails -> [Char]
buildSigJS siglnk@(SignatoryDetails {
  signatoryfstname
  , signatorysndname
  , signatorycompany
  , signatorynumber
  , signatoryemail
  , signatoryfstnameplacements
  , signatorysndnameplacements
  , signatorycompanyplacements
  , signatoryemailplacements
  , signatorynumberplacements
  , signatoryotherfields
  }) =
     "{ fstname: "  ++ jsStringFromBS  signatoryfstname
  ++ ", sndname: " ++ jsStringFromBS  signatorysndname
  ++ ", company: " ++ jsStringFromBS  signatorycompany
  ++ ", email: " ++ jsStringFromBS signatoryemail
  ++ ", number: " ++ jsStringFromBS signatorynumber
  ++ ", fstnameplacements: " ++ (jsArray (map buildPlacementJS signatoryfstnameplacements))
  ++ ", sndnameplacements: " ++ (jsArray (map buildPlacementJS signatorysndnameplacements))
  ++ ", companyplacements: " ++ (jsArray (map buildPlacementJS signatorycompanyplacements))
  ++ ", emailplacements: " ++ (jsArray (map buildPlacementJS signatoryemailplacements))
  ++ ", numberplacements: " ++ (jsArray (map buildPlacementJS signatorynumberplacements))
  ++ ", otherfields: " ++ (jsArray $ zipWith buildDefJS signatoryotherfields [1..])
  ++ " }"


buildJS :: SignatoryDetails -> [SignatoryDetails] -> [Char]
buildJS authordetails signatorydetails =
     "{ signatories: "
  ++ sigs
  ++ ", author: " ++ buildSigJS authordetails
  ++ " }"
  where
    sigs =
      if (length signatorydetails) > 0
         then jsArray (map buildSigJS signatorydetails)
         else jsArray [(buildSigJS emptyDetails)]


defaultInviteMessage :: BS.ByteString
defaultInviteMessage = BS.empty     


jsStringFromBS :: BS.ByteString -> String
jsStringFromBS bs =
  "\"" ++ (encode $ BS.toString bs) ++ "\""
  where
    encode ('"':ss) = "\\\"" ++ (encode ss)
    encode ('>':ss) = "\\>" ++ (encode ss)
    encode ('<':ss) = "\\<" ++ (encode ss)
    encode (s:ss) = s:(encode ss)
    encode [] = []
