{-# OPTIONS_GHC -Wall #-}

module Doc.DocView( emptyDetails
              , showFilesImages2
              , pageDocumentForAuthor
              , pageDocumentForViewer
              , pageDocumentList
              , landpageSignInviteView
              , landpageSendInviteView
              , landpageSignedView
              , landpageLoginForSaveView
              , landpageDocumentSavedView
              , pageDocumentForSign
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

landpageDocumentSavedView ::KontrakcjaTemplates -> IO String
landpageDocumentSavedView templates  = renderTemplate templates  "landpageDocumentSavedView" ()

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
       timetosignset = isJust documentdaystosign --swedish low constrain
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
     documentinfotext <- documentInfoText (ctxtemplates ctx) document Nothing author
     renderTemplate (ctxtemplates ctx) "pageDocumentForAuthorContent" $  
                                                              (setAttribute "documenttitle" $ BS.toString documenttitle) .
                                                              (setAttribute "documentid" $ show documentid) .
                                                              (setAttribute "linkissuedoc" $ show $ LinkIssueDoc documentid) .
                                                              (setAttribute "authorname" $ BS.toString $ signatoryname documentauthordetails) .
                                                              (setAttribute "authorcompany" $ BS.toString $ signatorycompany documentauthordetails ) .
                                                              (setAttribute "authornumber" $ BS.toString $ signatorynumber documentauthordetails) .
                                                              (setAttribute "authoremail" $ BS.toString $ signatoryemail documentauthordetails) .
                                                              (setAttribute "authorhaslink" $ authorhaslink) .                                                                              
                                                              (setAttribute "documentinvitetext" $ documentinvitetext) .                                                                              
                                                              (setAttribute "timetosignset" $ timetosignset) .
                                                              (setAttribute "invitationMailContent" $ invitationMailContent) .             
                                                              (setAttribute "documentdaystosignboxvalue" $ documentdaystosignboxvalue ) .
                                                              (setAttribute "anyinvitationundelivered" $ anyInvitationUndelivered document) .
                                                              (setAttribute "undelivered" $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document  ) .
                                                              (setAttribute "preparation" $ documentstatus == Preparation).
                                                              (setAttribute "signatories" signatories) .
                                                              (setAttribute "pending" $ documentstatus == Pending || documentstatus == AwaitingAuthor ) .
                                                              (setAttribute "awaitingauthor" $ documentstatus == AwaitingAuthor ) .
                                                              (setAttribute "canberestarted" $ documentstatus `elem` [Canceled , Timedout , Rejected]) . 
                                                              (setAttribute "restartForm" $ restartForm) .
                                                              (setAttribute "cancelMailContent" $ cancelMailContent)   .
                                                              (setAttribute "linkcancel" $ show $ LinkCancel document) .
                                                              (setAttribute "emailelegitimation" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)) .
                                                              (setAttribute "emailonly" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isNothing $ find (== ELegitimationIdentification) documentallowedidtypes)) .
                                                              (setAttribute "elegitimationonly" $ (isNothing $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)) .
                                                              (setAttribute "helpers" helpers) .
                                                              (setAttribute "docstate" (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks)) .
                                                              (setAttribute "linkissuedocpdf" $ show (LinkIssueDocPDF document)) .
                                                              (setAttribute "documentinfotext" $ documentinfotext)
   

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
       timetosignset = isJust documentdaystosign --swedish low constrain
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
     renderTemplate (ctxtemplates ctx) "pageDocumentForViewerContent" $  
                                                              (setAttribute "documenttitle" $ BS.toString documenttitle) .
                                                              (setAttribute "documentid" $ show documentid) .
                                                              (setAttribute "linkissuedoc" $ show $ LinkIssueDoc documentid) .
                                                              (setAttribute "authorname" $ BS.toString $ signatoryname documentauthordetails) .
                                                              (setAttribute "authorcompany" $ BS.toString $ signatorycompany documentauthordetails ) .
                                                              (setAttribute "authornumber" $ BS.toString $ signatorynumber documentauthordetails) .
                                                              (setAttribute "authoremail" $ BS.toString $ signatoryemail documentauthordetails) .
                                                              (setAttribute "authorhaslink" $ authorhaslink) .                                                                              
                                                              (setAttribute "documentinvitetext" $ documentinvitetext) .                                                                              
                                                              (setAttribute "timetosignset" $ timetosignset) .
                                                              (setAttribute "invitationMailContent" $ invitationMailContent) .             
                                                              (setAttribute "documentdaystosignboxvalue" $ documentdaystosignboxvalue ) .
                                                              (setAttribute "anyinvitationundelivered" $ anyInvitationUndelivered document) .
                                                              (setAttribute "undelivered" $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document  ) .
                                                              (setAttribute "preparation" $ documentstatus == Preparation).
                                                              (setAttribute "signatories" signatories) .
                                                              (setAttribute "pending" $ documentstatus == Pending || documentstatus == AwaitingAuthor ) .
                                                              (setAttribute "awaitingauthor" $ documentstatus == AwaitingAuthor ) .
                                                              (setAttribute "canberestarted" $ documentstatus `elem` [Canceled , Timedout , Rejected]) . 
                                                              (setAttribute "restartForm" $ restartForm) .
                                                              (setAttribute "cancelMailContent" $ cancelMailContent)   .
                                                              (setAttribute "linkcancel" $ show $ LinkCancel document) .
                                                              (setAttribute "emailelegitimation" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)) .
                                                              (setAttribute "emailonly" $ (isJust $ find (== EmailIdentification) documentallowedidtypes) && (isNothing $ find (== ELegitimationIdentification) documentallowedidtypes)) .
                                                              (setAttribute "elegitimationonly" $ (isNothing $ find (== EmailIdentification) documentallowedidtypes) && (isJust $ find (== ELegitimationIdentification) documentallowedidtypes)) .
                                                              (setAttribute "helpers" helpers) .
                                                              (setAttribute "docstate" (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks)) .
                                                              (setAttribute "linkissuedocpdf" $ show (LinkIssueDocPDF document))  .
                                                              (setAttribute "documentinfotext" $ documentinfotext)
 
                                                              
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
      reminderText <- if (wasSigned)
                      then renderTemplate ctxtemplates "reminderTextSigned" () 
                      else renderTemplate ctxtemplates "reminderTextNotSigned" ()
      reminderSenderText <- 
                     if (wasSigned)
                      then renderTemplate ctxtemplates "reminderSenderTextSigned" () 
                      else renderTemplate ctxtemplates "reminderSenderTextNotSigned" ()             
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
      renderTemplate ctxtemplates "showSignatoryLinkForSign" $  
                              (setAttribute "mainclass" $         if isCurrentSignatorAuthor  then "author" else "signatory") .
                              (setAttribute "status" $ status) .
                              (setAttribute "signatoryname" $     packToMString signatoryname ) .
                              (setAttribute "signatorycompany" $ packToMString signatorycompany) .
                              (setAttribute "signatorynumber" $   packToMString signatorynumber) .
                              (setAttribute "signatoryemail" $    packToMString signatoryemail) .
                              (setAttribute "fields" $ if documentstatus document == Closed 
                                                        then signatoryotherfields 
                                                        else []) .
                              (setAttribute "message" $  message) .
                              (setAttribute "reminderForm" $ reminderForm) .
                              (setAttribute "changeEmailAddress" $  changeEmailAddress) 


packToMString:: BS.ByteString -> Maybe String
packToMString x = if BS.null x then Nothing else (Just $ BS.toString x) 

pageDocumentForSign :: KontraLink 
                    -> Document 
                    -> Context
                    -> SignatoryLink
                    -> Bool 
                    -> User
                    -> IO String 
pageDocumentForSign action document ctx  invitedlink wassigned author =
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
     rejectMessage <- mailRejectMailContent (ctxtemplates ctx) Nothing ctx (prettyName author) document (personname invitedlink)                                                        
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
     renderTemplate (ctxtemplates ctx) "pageDocumentForSignContent" $
                 (setAttribute "helpers" helpers) .
                 (setAttribute "signatories" signatories) .
                 (setAttribute "messageoption" messageoption) .
                 (setAttribute "documenttitle" $ BS.toString $ documenttitle document) .
                 (setAttribute "authorname" $ BS.toString $ authorname) .
                 (setAttribute "rejectMessage" rejectMessage) .
                 (setAttribute "partyUnsigned" partyUnsigned) .
                 (setAttribute "action" $ show action) .
                 (setAttribute "title" $ BS.toString $ documenttitle document) .
                 (setAttribute "linkissuedocpdf" $ show (LinkIssueDocPDF document)) .
                 (setAttribute "docid" $ show $ documentid document) .
                 (setAttribute "documentinfotext" $ documentinfotext) .
                 (setAttribute "showflash" $ not wassigned && documentstatus document == Pending)

--Helper to get document after signing info text
documentInfoText::KontrakcjaTemplates->Document->(Maybe SignatoryLink) -> User -> IO String
documentInfoText templates document siglnk author =  
  
  renderTemplate templates "documentInfoText" $ 
                                                (setAttribute "notsignedbyme" $ isNothing $ join $ fmap maybesigninfo siglnk) .
                                                (setAttribute "signedbyme" $ isJust $ join $ fmap maybesigninfo siglnk) .
                                                (setAttribute "pending" $ documentstatus document == Pending) .
                                                (setAttribute "cancel" $ documentstatus document ==  Canceled) .
                                                (setAttribute "timedout" $ documentstatus document == Timedout) .
                                                (setAttribute "rejected" $ documentstatus document == Rejected) .
                                                (setAttribute "signed" $ documentstatus document == Closed) .
                                                (setAttribute "awaitingauthor" $ documentstatus document == AwaitingAuthor) . 
                                                (setAttribute "authorname" $ BS.toString $ userfullname author) 
                                               
                                                                                  





--We keep this javascript code generation for now
jsArray :: [[Char]] -> [Char]
jsArray xs = "[" ++ (joinWith ", " xs) ++ "]"

buildDefJS :: FieldDefinition -> [Char]
buildDefJS (FieldDefinition { fieldlabel, fieldvalue, fieldplacements }) = 
    "{ label: " ++ show fieldlabel -- show because we need quotes
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
