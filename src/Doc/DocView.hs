{-# OPTIONS_GHC -Wall #-}

module Doc.DocView( emptyDetails
              , showFilesImages2
              , pageDocumentForAuthor
              , pageDocumentForViewer
              , pageDocumentForSignatory
              , pageDocumentList
              , landpageSignInviteView
              , landpageSendInviteView
              , landpageSignedView
              , landpageLoginForSaveView
              , flashRemindMailSent
              , flashMessageCanceled 
              , flashDocumentRestarted
              , flashDocumentDraftSaved
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
              ) where
import Doc.DocState
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Kontra
import KontraLink
import Misc
import MinutesTime
import Doc.DocViewMail
import Doc.DocViewUtil
import Templates.Templates
import Templates.TemplatesUtils
import Mails.MailsUtil
import User.UserView (prettyName,UserSmallView(..))
import Data.Typeable
import Data.Data
import Data.List (find)
import Control.Monad (join)
import Data.Maybe


landpageSignInviteView ::KontrakcjaTemplates -> Document ->  IO String
landpageSignInviteView templates  document =
     do 
      partylist <-renderListTemplate templates (map (BS.toString . personname') $ partyListButAuthor document)
      renderTemplate templates  "landpageSignInviteView" [("partyListButAuthor", partylist),
                                                          ("documenttitle",BS.toString $ documenttitle document )]

landpageSendInviteView ::KontrakcjaTemplates -> Document ->  IO String
landpageSendInviteView templates  document =
     do 
      partylist <-renderListTemplate templates (map (BS.toString . personname') $ partyListButAuthor document)
      renderTemplate templates  "landpageSendInviteView" [("partyListButAuthor", partylist),
                                                          ("documenttitle",BS.toString $ documenttitle document )]

willCreateAccountForYou::KontrakcjaTemplates -> Document->SignatoryLink->Bool->  IO String
willCreateAccountForYou templates  document siglink hasAccount = 
    if (hasAccount)
      then renderTemplate templates  "willCreateAccountForYouHasAccount" $ do
               field "email" $ signatoryemail $ signatorydetails siglink   
      else renderTemplate templates "willCreateAccountForYouNoAccount" $  do
               field "documentid" $ show $ unDocumentID $ documentid document
               field "signatorylinkid" $ unSignatoryLinkID $ signatorylinkid siglink
               
              
landpageRejectedView ::KontrakcjaTemplates -> Document -> IO String
landpageRejectedView templates document =
   do 
      partylist <-renderListTemplate templates  (map (BS.toString . personname') $ partyList document)
      renderTemplate templates  "landpageRejectedView" [("partyList", partylist),
                                              ("documenttitle",BS.toString $ documenttitle document )]

landpageSignedView ::KontrakcjaTemplates -> Document -> SignatoryLink -> Bool -> IO String
landpageSignedView templates document@Document{documenttitle,documentstatus} signatorylink hasaccount =
     if (documentstatus == Closed) 
      then renderTemplate templates "landpageSignedViewClosed" $ do 
                field "partyListString" $ renderListTemplate templates $ map (BS.toString . personname') $ partyList document
                field "documenttitle" $ BS.toString $ documenttitle
                field "willCreateAccountForYou" $ willCreateAccountForYou templates document signatorylink hasaccount 
      else renderTemplate templates "landpageSignedViewNotClosed" $ do  
                field "partyUnsignedListString" $ renderListTemplate templates  $ map (BS.toString . personname') $ partyUnsignedList document
                field "documenttitle" $ BS.toString $ documenttitle
                field "willCreateAccountForYou" $ willCreateAccountForYou templates document signatorylink hasaccount 

landpageLoginForSaveView::KontrakcjaTemplates ->IO String
landpageLoginForSaveView  templates  = renderTemplate templates  "landpageLoginForSaveView" ()

flashDocumentDraftSaved :: KontrakcjaTemplates ->IO String
flashDocumentDraftSaved  templates  = renderTemplate templates  "flashDocumentDraftSaved" ()

flashDocumentRestarted :: KontrakcjaTemplates ->IO String
flashDocumentRestarted  templates  = renderTemplate templates "flashDocumentRestarted" ()

flashRemindMailSent :: KontrakcjaTemplates -> SignatoryLink -> IO String                                
flashRemindMailSent templates  signlink@SignatoryLink{maybesigninfo = Nothing}  = 
                            renderTemplate templates  "flashRemindMailSentNotSigned" [("personname",BS.toString $ personname signlink)] 
flashRemindMailSent templates  signlink = 
                            renderTemplate templates  "flashRemindMailSentSigned" [("personname",BS.toString $ personname signlink)] 


flashMessageCanceled :: KontrakcjaTemplates -> IO String
flashMessageCanceled templates = renderTemplate templates  "flashMessageCanceled" ()


--All doc view
singLinkUserSmallView::SignatoryLink -> UserSmallView
singLinkUserSmallView sl = UserSmallView {     usvId =  show $ signatorylinkid sl
                                             , usvFullname = BS.toString $ personname sl
                                             , usvEmail = ""
                                             , usvDocsCount = "" }

data DocumentSmallView = DocumentSmallView {
                          dsvId::String,
                          dsvTitle::String,
                          dsvSignatories::[UserSmallView],
                          dsvAnyinvitationundelivered::Bool,
                          dsvStatusimage::String,
                          dsvDoclink::String,
                          dsvDavelink::Maybe String,
                          dsvTimeoutdate::Maybe String,
                          dsvTimeoutdaysleft::Maybe String,  
                          dsvMtime::String,
                          dsvIsauthor::Bool  
                         } deriving (Data, Typeable)
                         
documentSmallView::MinutesTime ->  User -> Document ->DocumentSmallView
documentSmallView crtime user doc = DocumentSmallView {
                          dsvId = show $ documentid doc,
                          dsvTitle = BS.toString $ documenttitle doc,
                          dsvSignatories = map singLinkUserSmallView $ documentsignatorylinks doc,
                          dsvAnyinvitationundelivered = anyInvitationUndelivered doc,
                          dsvStatusimage = "/theme/images/" ++
                                               case (documentstatus doc) of
                                                  Preparation -> "status_draft.png"
                                                  Closed -> "status_signed.png"
                                                  Canceled -> "status_rejected.png"
                                                  Timedout -> "status_timeout.png"
                                                  Rejected -> "status_rejected.png"
                                                  -- Withdrawn -> "status_rejected.png"
                                                  Pending  -> if anyInvitationUndelivered doc
                                                               then "status_rejected.png"    
                                                               else if all (isJust . maybeseeninfo) $ documentsignatorylinks doc
                                                                     then "status_viewed.png"
                                                                     else "status_pending.png"
                                                  AwaitingAuthor -> if anyInvitationUndelivered doc
                                                                     then "status_rejected.png"    
                                                                     else if all (isJust . maybeseeninfo) $ documentsignatorylinks doc
                                                                           then "status_viewed.png"
                                                                           else "status_pending.png"  
                                                  _ -> "status_rejected.png",       
                          
                          dsvDoclink =     if (unAuthor $ documentauthor doc) ==(userid user) || (null $ signatorylinklist)
                                            then show $ LinkIssueDoc $ documentid doc
                                            else show $ LinkSignDoc doc (head $ signatorylinklist),
                          dsvDavelink = if isSuperUser (Just user) 
                                         then Just $ "/dave/document/" ++ (show documentid) 
                                         else Nothing  ,               
                          dsvTimeoutdate =  fromTimeout show,
                          dsvTimeoutdaysleft =  fromTimeout $ show . (dateDiffInDays crtime),    
                          dsvMtime = showDateAbbrev crtime (documentmtime doc),
                          dsvIsauthor = isAuthor doc user
                         }
  where   signatorylinklist = filter (isMatchingSignatoryLink user) $ documentsignatorylinks doc  
          fromTimeout f =  case (documenttimeouttime doc,documentstatus doc) of
                                (Just (TimeoutTime x),Pending) -> Just $ f x
                                _ -> Nothing
                                                                            

pageDocumentList:: KontrakcjaTemplates -> MinutesTime -> User -> [Document] -> IO String
pageDocumentList templates ctime user documents = renderTemplate templates "pageDocumentList" $
                                                        (setAttribute "documents" $ map (documentSmallView ctime user) $ filter (not . documentdeleted) documents)


    
showFileImages ::KontrakcjaTemplates -> File -> JpegPages -> IO String
showFileImages templates File{fileid} (JpegPages jpgpages) = renderTemplate templates "showFileImagesReady" $
                                                              (setAttribute "fileid" $ show fileid) .
                                                              (setAttribute "pages" $ [1..(length jpgpages)])                                                          
showFileImages templates _ JpegPagesPending = renderTemplate templates  "showFileImagesPending" ()    
showFileImages templates _ (JpegPagesError normalizelog) = renderTemplate templates  "showFileImagesError" [("normalizelog",BS.toString normalizelog)]
  
     
showFilesImages2 :: KontrakcjaTemplates ->  [(File, JpegPages)] -> IO String
showFilesImages2 templates files = do
                                    filesPages <- sequence $ map (uncurry (showFileImages templates)) files
                                    renderTemplate templates  "spanNoEscape" [("it",concat filesPages)]  


{-

   Document is invalid
   Fel filformat
   Vi beklagar, fel filformat

   mp3 -- we cannot do anything with this document
-}

emptyDetails :: SignatoryDetails
emptyDetails = SignatoryDetails 
          { signatoryname = BS.empty
          , signatorycompany = BS.empty
          , signatorynumber = BS.empty
          , signatoryemail = BS.empty
          , signatorynameplacements = []
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
             -> IO String
pageDocumentForAuthor ctx
             document@Document{ documentsignatorylinks
                              , documenttitle
                              , documentid
                              , documentstatus
                              , documentdaystosign
                              , documentinvitetext
                              , documentallowedidtypes
                              } 
             author =
   let 
       authorid = userid author
       authorhaslink = not $ null $ filter (not . isNotLinkForUserID authorid) documentsignatorylinks
       documentdaystosignboxvalue = maybe 7 id documentdaystosign
       documentauthordetails = (signatoryDetailsFromUser author) 
                               {
                                 signatoryemailplacements = (authoremailplacements document)
                               , signatorynameplacements = (authornameplacements document)
                               , signatorycompanyplacements = (authorcompanyplacements document)
                               , signatorynumberplacements = (authornumberplacements document)
                               , signatoryotherfields = (authorotherfields document)
                               }
   in do
     helpers <- renderTemplate (ctxtemplates ctx) "pageDocumentForAuthorHelpers" [("documentid",show documentid)] 
     signatories <- fmap concat $ sequence $ map (showSignatoryLinkForSign ctx document author) documentsignatorylinks                                                                                  
     invitationMailContent <- mailInvitationToSignContent (ctxtemplates ctx) False ctx document author Nothing
     restartForm <-   renderActionButton  (ctxtemplates ctx) (LinkRestart documentid) "restartButtonName"
     cancelMailContent <- mailCancelDocumentByAuthorContent  (ctxtemplates ctx) False Nothing ctx document author
     documentinfotext <- documentInfoText (ctxtemplates ctx) document (find (isMatchingSignatoryLink author) documentsignatorylinks) author
     renderTemplate (ctxtemplates ctx) "pageDocumentForAuthorContent" $  do 
                 field "documenttitle" $ BS.toString documenttitle
                 field "documentid" $ show documentid
                 field "linkissuedoc" $ show $ LinkIssueDoc documentid
                 field "authorhaslink" $ authorhaslink                               
                 field "documentinvitetext" $ documentinvitetext                               
                 field "invitationMailContent" $ invitationMailContent           
                 field "documentdaystosignboxvalue" $ documentdaystosignboxvalue 
                 field "anyinvitationundelivered" $ anyInvitationUndelivered document
                 field "undelivered" $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document 
                 field "signatories" signatories
                 field "canberestarted" $ documentstatus `elem` [Canceled , Timedout , Rejected] 
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
                 documentInfoFields document author

{- |
   Show the document for Viewers (friends of author or signatory).
   Show no buttons or other controls
 -}                                                                                                          

pageDocumentForViewer :: Context -> Document -> User -> IO String
pageDocumentForViewer ctx
             document@Document{ documentsignatorylinks
                              , documenttitle
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
       documentauthordetails = (signatoryDetailsFromUser author)
                               {
                                 signatoryemailplacements = (authoremailplacements document)
                               , signatorynameplacements = (authornameplacements document)
                               , signatorycompanyplacements = (authorcompanyplacements document)
                               , signatorynumberplacements = (authornumberplacements document)
                               , signatoryotherfields = (authorotherfields document)
                               }
   in do
     helpers <- renderTemplate (ctxtemplates ctx) "pageDocumentForViewerHelpers" [("documentid",show documentid) ] 
     signatories <- fmap concat $ sequence $ map (showSignatoryLinkForSign ctx document author) documentsignatorylinks                                                                                  
     invitationMailContent <- mailInvitationToSignContent (ctxtemplates ctx) False ctx document author Nothing
     restartForm <-   renderActionButton  (ctxtemplates ctx) (LinkRestart documentid) "restartButtonName"
     cancelMailContent <- mailCancelDocumentByAuthorContent  (ctxtemplates ctx) False Nothing ctx document author
     documentinfotext <- documentInfoText (ctxtemplates ctx) document Nothing author
     renderTemplate (ctxtemplates ctx) "pageDocumentForViewerContent" $  do 
                  field "documenttitle" $ BS.toString documenttitle
                  field "documentid" $ show documentid
                  field "linkissuedoc" $ show $ LinkIssueDoc documentid
                  field "authorhaslink" $ authorhaslink                                                                            
                  field "documentinvitetext" $ documentinvitetext                                                                            
                  field "invitationMailContent" $ invitationMailContent             
                  field "documentdaystosignboxvalue" $ documentdaystosignboxvalue 
                  field "anyinvitationundelivered" $ anyInvitationUndelivered document
                  field "undelivered" $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document 
                  field "signatories" signatories
                  field "canberestarted" $ documentstatus `elem` [Canceled , Timedout , Rejected]
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
                  documentInfoFields document author
 
pageDocumentForSignatory :: KontraLink 
                    -> Document 
                    -> Context
                    -> SignatoryLink
                    -> Bool 
                    -> User
                    -> IO String 
pageDocumentForSignatory action document ctx  invitedlink wassigned author =
    let
       localscripts = "var docstate = " ++ 
                      (buildJS documentauthordetails $ 
                       map signatorydetails (documentsignatorylinks document)) ++ 
                      "; docstate['useremail'] = '" ++ 
                      (BS.toString $ signatoryemail $ signatorydetails invitedlink) ++ 
                      "';"       
       magichash = signatorymagichash invitedlink
       authorname = signatoryname documentauthordetails
       allbutinvited = filter (/= invitedlink) (documentsignatorylinks document)
       documentauthordetails = signatoryDetailsFromUser author
       allowedtypes = documentallowedidtypes document
    in
    do 
     helpers <- renderTemplate (ctxtemplates ctx) "pageDocumentForSignHelpers" [("documentid",show (documentid document)),("localscripts",localscripts)]   
     rejectMessage <- mailRejectMailContent (ctxtemplates ctx) Nothing ctx (prettyName author) document invitedlink                                                        
     signatories <- fmap concat $ sequence $ map (showSignatoryLinkForSign ctx document author) (invitedlink : allbutinvited)
     messageoption <- caseOf [ 
                     (wassigned,                           renderTemplate (ctxtemplates ctx) "pageDocumentForSignSigned" ()),  
                     (documentstatus document == Timedout, renderTemplate (ctxtemplates ctx) "pageDocumentForSignTimedout" ()),
                     (documentstatus document == Pending,  renderTemplate (ctxtemplates ctx) "pageDocumentForSignButtons" $ 
                      (setAttribute "emailallowed" $  isJust $ find (== EmailIdentification) allowedtypes) .
                      (setAttribute "elegallowed" $ isJust $ find (== ELegitimationIdentification) allowedtypes))
                     ]  $ return ""   
                     
     partyUnsigned <- renderListTemplate (ctxtemplates ctx) $  map (BS.toString . personname') $ partyUnsignedMeAndList magichash document
     documentinfotext <- documentInfoText (ctxtemplates ctx) document (Just invitedlink) author
     renderTemplate (ctxtemplates ctx) "pageDocumentForSignContent" $ do
                 field "helpers" helpers
                 field "signatories" signatories
                 field "messageoption" messageoption
                 field "documenttitle" $ BS.toString $ documenttitle document
                 field "rejectMessage" rejectMessage
                 field "partyUnsigned" partyUnsigned
                 field "action" $ show action
                 field "title" $ BS.toString $ documenttitle document
                 field "linkissuedocpdf" $ show (LinkIssueDocPDF document)
                 field "docid" $ show $ documentid document
                 field "documentinfotext" $ documentinfotext
                 documentInfoFields document author
                 signedByMeFields document (Just invitedlink)


--- Display of signatory                                                             
showSignatoryLinkForSign::Context -> Document -> User -> SignatoryLink-> IO String
showSignatoryLinkForSign ctx@(Context {ctxmaybeuser = muser,ctxtemplates})  document author siglnk@(SignatoryLink{  signatorylinkid 
                                       , maybesigninfo
                                       , maybeseeninfo
                                       , invitationdeliverystatus
                                       , signatorydetails = SignatoryDetails
                                                            { signatoryname
                                                            , signatorynumber
                                                            , signatorycompany
                                                            , signatoryemail
                                                            , signatoryotherfields
                                                            }
                                         }) =
  do
      let isCurrentUserAuthor = maybe False (isAuthor document) muser
      let isCurrentSignatorAuthor = (fmap (unEmail . useremail . userinfo) muser) ==  (Just signatoryemail)      
      let wasSigned =  isJust maybesigninfo && (not $ isCurrentSignatorAuthor && (documentstatus document == AwaitingAuthor))
      let wasSeen = isJust maybeseeninfo
      let isTimedout = documentstatus document == Timedout
      let isCanceled = documentstatus document == Canceled
      let isRejected = documentstatus document == Rejected
      let isClosed = documentstatus document == Closed

      -- let isWithDrawn = documentstatus document == Withdrawn
      let dontShowAnyReminder = isTimedout || isCanceled || isRejected
      let dialogHeight =   if (wasSigned) then "400" else "600"
      let status =  caseOf
                [
                ( invitationdeliverystatus == Undelivered,"/theme/images/status_rejected.png"),
                -- ( isWithDrawn,"/theme/images/status_rejected.png"), 
                ( isCanceled, "/theme/images/status_rejected.png"), 
                ( isRejected, "/theme/images/status_rejected.png"), 
                ( isTimedout, "/theme/images/status_timeout.png"), 
                ( wasSigned,  "/theme/images/status_signed.png"),
                ( wasSeen,    "/theme/images/status_viewed.png" )]        
                              "/theme/images/status_pending.png"
      message <- caseOf
                   [
                    (wasSigned, renderTemplate ctxtemplates "signatoryMessageSigned" [("date", showDateOnly $ signtime $ fromJust maybesigninfo)]),  
                    (isTimedout, renderTemplate ctxtemplates "signatoryMessageTimedout" ()),
                    (isCanceled || isRejected, return "" ),
                    (wasSeen,  renderTemplate ctxtemplates "signatoryMessageSeen" [("date", showDateOnly $ signtime $ fromJust maybeseeninfo)])
                   ]        (renderTemplate ctxtemplates "signatoryMessageNotSigned" ())
      reminderText <- if (isClosed)
                      then renderTemplate ctxtemplates "reminderTextSigned" () 
                      else if (not wasSigned)
                           then renderTemplate ctxtemplates "reminderTextNotSigned" ()
                           else return ""
      reminderSenderText <- 
                     if (isClosed)
                      then renderTemplate ctxtemplates "reminderSenderTextSigned" () 
                      else if (not wasSigned)
                           then renderTemplate ctxtemplates "reminderSenderTextNotSigned" ()             
                           else return ""
      reminderEditorText <- renderTemplate ctxtemplates "reminderEditorText" ()                         
      reminderDialogTitle <- return reminderText   
      reminderMessage <-  mailDocumentRemindContent ctxtemplates Nothing ctx document siglnk author
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
                                      
      changeEmailAddress <-  whenMaybe (isCurrentUserAuthor && (invitationdeliverystatus == Undelivered) && (not dontShowAnyReminder)) $ 
                                renderTemplate ctxtemplates "changeEmailAddress" $  
                                         (setAttribute "linkchangeemail" $  show $ LinkChangeSignatoryEmail (documentid document) signatorylinkid) .
                                         (setAttribute "signatoryemail" $  BS.toString signatoryemail)          

      let  signatoryFields =  if documentstatus document == Closed then signatoryotherfields else []                                       
      renderTemplate ctxtemplates "showSignatoryLinkForSign" $  do
                              field "mainclass" $         if isCurrentSignatorAuthor  then "author" else "signatory"
                              field "status" $ status
                              field "signatoryname" $     packToMString signatoryname 
                              field "signatorycompany" $ packToMString signatorycompany
                              field "signatorynumber" $   packToMString signatorynumber
                              field "signatoryemail" $    packToMString signatoryemail
                              field "fields" $ for signatoryFields $ \sof -> do
                                                           field "fieldlabel" (fieldlabel sof)
                                                           field "fieldvalue" (fieldvalue sof)
                              field "message" $  message
                              field "reminderForm" $ reminderForm
                              field "changeEmailAddress" $  changeEmailAddress

packToMString:: BS.ByteString -> Maybe String
packToMString x = if BS.null x then Nothing else (Just $ BS.toString x) 


--Helper to get document after signing info text
documentInfoText::KontrakcjaTemplates->Document->(Maybe SignatoryLink) -> User -> IO String
documentInfoText templates document siglnk author =
  renderTemplate templates "documentInfoText" $ do
    documentInfoFields document author 
    signedByMeFields document siglnk
    
    

documentInfoFields::Document -> User -> Fields   
documentInfoFields  document author  = do 
    field "authorname" $ BS.toString $ userfullname author
    field "timetosignset" $  isJust $ documentdaystosign document
    documentStatusFields document
    
documentStatusFields::Document -> Fields    
documentStatusFields document  = do    
    field "preparation" $ documentstatus document == Preparation
    field "pending" $ documentstatus document == Pending
    field "cancel" $ documentstatus document ==  Canceled
    field "timedout" $ documentstatus document == Timedout
    field "rejected" $ documentstatus document == Rejected
    field "signed" $ documentstatus document == Closed
    field "awaitingauthor" $ documentstatus document == AwaitingAuthor
    

signedByMeFields :: Document -> (Maybe SignatoryLink) -> Fields
signedByMeFields document siglnk = do 
    field "notsignedbyme" $ (isJust siglnk) && (isNothing $ maybesigninfo $ fromJust siglnk)
    field "signedbyme" $ (isJust siglnk) && (isJust $ maybesigninfo $ fromJust siglnk)
    field "iamauthor" $ isAuthor document siglnk
    
 
uploadPage:: KontrakcjaTemplates -> IO String
uploadPage templates = do
                   uploadBox <- renderTemplate templates "uploadPageContent" ()
                   renderTemplate templates "creatingDocumentFrame" $ 
                                                       (setAttribute  "steps" uploadBox) .
                                                       (setAttribute  "step1" True) .
                                                       (setAttribute  "submitFormOnNext" True) 

--We keep this javascript code generation for now
jsArray :: [[Char]] -> [Char]
jsArray xs = "[" ++ (joinWith ", " xs) ++ "]"

buildDefJS :: FieldDefinition -> [Char]
buildDefJS (FieldDefinition { fieldlabel, fieldvalue, fieldplacements }) = 
    "{ label: " ++ jsStringFromBS fieldlabel -- show because we need quotes
                    ++ ", value: " ++ jsStringFromBS fieldvalue
                    ++ ", placements: " ++ (jsArray (map buildPlacementJS fieldplacements))
                    ++ " }"
                    
buildPlacementJS :: FieldPlacement -> [Char]
buildPlacementJS (FieldPlacement { placementx, placementy, placementpage, placementpagewidth, placementpageheight }) = 
    "{ x: " ++ show placementx 
                ++ ", y: " ++ show placementy
                ++ ", page: " ++ show placementpage
                ++ ", h: " ++ show placementpageheight
                ++ ", w: " ++ show placementpagewidth
                ++ " }"
                
buildSigJS :: SignatoryDetails -> [Char]
buildSigJS (SignatoryDetails { signatoryname, signatorycompany, signatorynumber, signatoryemail, signatorynameplacements, signatorycompanyplacements, signatoryemailplacements, signatorynumberplacements, signatoryotherfields }) = 
                      "{ name: " ++ jsStringFromBS  signatoryname
                   ++ ", company: " ++ jsStringFromBS  signatorycompany
                   ++ ", email: " ++ jsStringFromBS signatoryemail
                   ++ ", number: " ++ jsStringFromBS signatorynumber
                   ++ ", nameplacements: " ++ (jsArray (map buildPlacementJS signatorynameplacements))
                   ++ ", companyplacements: " ++ (jsArray (map buildPlacementJS signatorycompanyplacements))
                   ++ ", emailplacements: " ++ (jsArray (map buildPlacementJS signatoryemailplacements))
                   ++ ", numberplacements: " ++ (jsArray (map buildPlacementJS signatorynumberplacements))
                   ++ ", otherfields: " ++ (jsArray (map buildDefJS signatoryotherfields))
                   ++ " }"
                   
buildJS :: SignatoryDetails -> [SignatoryDetails] -> [Char]
buildJS authordetails signatorydetails = 
    "{ signatories: " ++ sigs
                          ++ ", author: " ++ buildSigJS authordetails
                          ++ " }" where 
                              sigs = if (length signatorydetails) > 0
                                     then (jsArray (map buildSigJS signatorydetails))
                                     else (jsArray [(buildSigJS emptyDetails)])
                                    
defaultInviteMessage :: BS.ByteString
defaultInviteMessage = BS.empty     

jsStringFromBS::BS.ByteString -> String
jsStringFromBS bs =  "\""++(encode $ BS.toString bs)++"\""
  where  encode ('"':ss) = "\\\"" ++ (encode ss)
         encode ('>':ss) = "\\>" ++ (encode ss)
         encode ('<':ss) = "\\<" ++ (encode ss)
         encode (s:ss) = s:(encode ss)
         encode [] = []
