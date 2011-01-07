{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}

module DocView( emptyDetails
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
              , mailDocumentRejectedForAuthor
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
import Data.List
import DocState
import HSP
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified HSX.XMLGenerator as HSX
import User
import KontraLink
import Misc
import MinutesTime
import Data.Maybe
import DocViewMail
import DocViewUtil
import Templates.Templates
import Templates.TemplatesUtils
import Mails.MailsUtil
import UserView (prettyName,UserSmallView(..))
import Data.Typeable
import Data.Data

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
willCreateAccountForYou templates  _ _ False =  renderTemplate templates "willCreateAccountForYouNoAccount" ([]::[(String,String)])
willCreateAccountForYou templates  document siglink True = 
                                     renderTemplate templates  "willCreateAccountForYouHasAccount" 
                                                                     [("documentid",show $ unDocumentID $ documentid document),
                                                                     ("signatorylinkid",show $ unSignatoryLinkID $ signatorylinkid siglink)]

landpageRejectedView ::KontrakcjaTemplates -> Document -> IO String
landpageRejectedView templates document =
   do 
      partylist <-renderListTemplate templates  (map (BS.toString . personname') $ partyList document)
      renderTemplate templates  "landpageRejectedView" [("partyList", partylist),
                                              ("documenttitle",BS.toString $ documenttitle document )]

landpageSignedView ::KontrakcjaTemplates -> Document -> SignatoryLink -> Bool -> IO String
landpageSignedView templates document@Document{documenttitle,documentstatus} signatorylink hasaccount =
    do
       willCreateAccountForYouProposal <- willCreateAccountForYou templates document signatorylink (not hasaccount) 
       if (documentstatus == Closed) 
        then do
              partylist <- renderListTemplate templates $ map (BS.toString . personname') $ partyList document
              renderTemplate templates "landpageSignedViewClosed" [("partyListString", partylist),
                                                         ("documenttitle",BS.toString $ documenttitle),
                                                         ("willCreateAccountForYou", willCreateAccountForYouProposal)]
        else do
              partyunsignedlist <- renderListTemplate templates  $ map (BS.toString . personname') $ partyUnsignedList document
              renderTemplate templates  "landpageSignedViewNotClosed"  [("partyUnsignedListString", partyunsignedlist),
                                                             ("documenttitle",BS.toString $ documenttitle),
                                                             ("willCreateAccountForYou", willCreateAccountForYouProposal)]   

landpageLoginForSaveView::KontrakcjaTemplates ->IO String
landpageLoginForSaveView  templates  = renderTemplate templates  "landpageLoginForSaveView" []

landpageDocumentSavedView ::KontrakcjaTemplates -> IO String
landpageDocumentSavedView templates  = renderTemplate templates  "landpageDocumentSavedView" []

flashDocumentDraftSaved :: KontrakcjaTemplates ->IO String
flashDocumentDraftSaved  templates  = renderTemplate templates  "flashDocumentDraftSaved" []

flashDocumentRestarted :: KontrakcjaTemplates ->IO String
flashDocumentRestarted  templates  = renderTemplate templates "flashDocumentRestarted" []

flashRemindMailSent :: KontrakcjaTemplates -> SignatoryLink -> IO String                                
flashRemindMailSent templates  signlink@SignatoryLink{maybesigninfo = Nothing}  = 
                            renderTemplate templates  "flashRemindMailSentNotSigned" [("personname",BS.toString $ personname signlink)] 
flashRemindMailSent templates  signlink = 
                            renderTemplate templates  "flashRemindMailSentSigned" [("personname",BS.toString $ personname signlink)] 


flashMessageCanceled :: KontrakcjaTemplates -> IO String
flashMessageCanceled templates = renderTemplate templates  "flashMessageCanceled" []


--All doc view
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
                          dsvMtime::String
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
                                                  Pending  -> if  any (isJust . maybeseeninfo) $ documentsignatorylinks doc
                                                               then "status_viewed.png"
                                                               else "status_pending.png"
                                                  AwaitingAuthor -> "status_pending.png"
                                                  Closed -> "status_signed.png"
                                                  Canceled -> "status_rejected.png"
                                                  Timedout -> "status_timeout.png"
                                                  Rejected -> "status_rejected.png"
                                                  Withdrawn -> "status_rejected.png",
                          dsvDoclink =     if (unAuthor $ documentauthor doc) ==(userid user) || (null $ signatorylinklist)
                                            then show $ LinkIssueDoc $ documentid doc
                                            else show $ LinkSignDoc doc (head $ signatorylinklist),
                          dsvDavelink = if isSuperUser (Just user) 
                                         then Just $ "/dave/document/" ++ (show documentid) 
                                         else Nothing  ,               
                          dsvTimeoutdate =  fromTimeout show,
                          dsvTimeoutdaysleft =  fromTimeout $ show . (dateDiffInDays crtime),    
                          dsvMtime = showDateAbbrev crtime (documentmtime doc)
                         }
  where   signatorylinklist = filter (isMatchingSignatoryLink user) $ documentsignatorylinks doc  
          fromTimeout f =  case (documenttimeouttime doc,documentstatus doc) of
                                (Just (TimeoutTime x),Pending) -> Just $ f x
                                _ -> Nothing
                                                                            

pageDocumentList:: KontrakcjaTemplates -> MinutesTime -> User -> [Document] -> IO String
pageDocumentList templates ctime user documents = renderTemplateComplex templates "pageDocumentList" $
                                                        (setAttribute "documents" $ map (documentSmallView ctime user) $ filter (not . documentdeleted) documents)



----Single document view
showSignatoryEntryForEdit :: KontrakcjaTemplates -> DocState.SignatoryDetails -> IO String
showSignatoryEntryForEdit templates (SignatoryDetails{signatoryname,signatorycompany,signatorynumber, signatoryemail}) = 
    showSignatoryEntryForEdit2 templates "" 
                                  (BS.toString signatoryname) 
                                  (BS.toString signatorycompany) 
                                  (BS.toString signatorynumber) 
                                  (BS.toString signatoryemail)

showSignatoryEntryForEdit2 :: KontrakcjaTemplates -> String -> String -> String -> String -> String -> IO String
showSignatoryEntryForEdit2 templates idx signatoryname signatorycompany signatorynumber signatoryemail = 
 renderTemplateComplex templates "showSignatoryEntryForEdit2" $  
                                                              (setAttribute "idx" $ idx) .
                                                              (setAttribute "signatoryname" $ signatoryname) .
                                                              (setAttribute "signatorycompany" $ signatorycompany) .
                                                              (setAttribute "signatorynumber" $ signatorynumber) .
                                                              (setAttribute "signatoryemail" $ signatoryemail) 
                                                              
    
showFileImages ::KontrakcjaTemplates -> File -> JpegPages -> IO String
showFileImages templates File{fileid} (JpegPages jpgpages) = renderTemplateComplex templates "showFileImagesReady" $
                                                              (setAttribute "fileid" $ show fileid) .
                                                              (setAttribute "pages" $ [1..(length jpgpages)])                                                          
showFileImages templates _ JpegPagesPending = renderTemplate templates  "showFileImagesPending" []    
showFileImages templates _ (JpegPagesError normalizelog) = renderTemplate templates  "showFileImagesError" [("normalizelog",BS.toString normalizelog)]
  
     
showFilesImages2 :: KontrakcjaTemplates ->  [(File, JpegPages)] -> IO String
showFilesImages2 templates files = do
                                    filesPages <- sequence $ map (uncurry (showFileImages templates)) files
                                    renderTemplate templates  "span" [("it",concat filesPages)]  


showDocumentBox :: KontrakcjaTemplates ->  IO String
showDocumentBox templates = renderTemplate templates  "showDocumentBox" []


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

{- |
   Show the document to the author with controls he needs.
 -}
pageDocumentForAuthor :: Context 
             -> Document 
             -> User
             -> (HSPT IO XML) 
pageDocumentForAuthor ctx
             document@Document{ documentsignatorylinks
                              , documenttitle
                              , documentid
                              , documentstatus
                              , documentdaystosign
                              , documentinvitetext
                              } 
             author =
   let helper = [  <span class="templating_insert"> <%fmap cdata $ showSignatoryEntryForEdit2 (ctxtemplates ctx) "signatory_template" "" "" "" "" %></span>,
                  <script> var documentid = "<% show $ documentid %>"; 
                  </script>
                ]
       authorid = userid author
       -- the author gets his own space when he's editing
       allinvited = filter (isNotLinkForUserID authorid) documentsignatorylinks
       authorhaslink = not $ null $ filter (not . isNotLinkForUserID authorid) documentsignatorylinks
       documentdaystosignboxvalue = maybe 7 id documentdaystosign
       timetosignset = isJust documentdaystosign --swedish low constrain
       documentauthordetails = signatoryDetailsFromUser author
   in showDocumentPageHelper (ctxtemplates ctx) document helper 
           (documenttitle)  
      <div>
       <div id="loading-message" style="display:none">
            Loading pages . . .
       </div>
       <div id="edit-bar">
        -- someone please refactor this. the then statement is so long I can't see the else!
        <%if documentstatus == Preparation
           then 
             <span>
               <script type="text/javascript">
                 <% "var docstate = " ++ (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks) ++ ";" %>
               </script>
              <form method="post" name="form" action=(LinkIssueDoc documentid) id="main-document-form"> 
              Avsändare<br/>
              <div style="margin-bottom: 10px;" id="authordetails">
              <strong><span id="sauthorname"><% addbr $ signatoryname documentauthordetails %></span></strong>
              <span id="sauthorcompany"><% addbr $ signatorycompany documentauthordetails %></span>
              <span id="sauthornumber"><% addbr $ signatorynumber documentauthordetails %></span>
              <span id="sauthoremail"><% addbr $ signatoryemail documentauthordetails %></span>
              </div>

              Användarroll 
              <% if authorhaslink 
                  then
                      <select name="authorrole" id="authorroledropdown">
                                  <option value="signatory">Undertecknare</option>
                                  <option value="secretary">Sekreterare</option>
                      </select>
                  else
                      <select name="authorrole" id="authorroledropdown">
                                  <option value="secretary">Sekreterare</option>
                                  <option value="signatory">Undertecknare</option>
                      </select> %>

              <br /><br />

              Motpart<br/>
              <div id="signatorylist">
               <% map ((fmap cdata) . showSignatoryEntryForEdit (ctxtemplates ctx)) (if null allinvited
                                                 then [emptyDetails] 
                                                 else map signatorydetails allinvited) %>
              </div>
              <small><a id="addsiglink" onclick="signatoryadd(); return false;" href="#">Lägg till fler</a></small>
              <div style="margin-top: 20px">
              <small><a rel="#edit-invite-text-dialog" id="editinvitetextlink" href="#" style="padding-top:3px">Hälsningsmeddelande</a></small>
              <input type="hidden" id="invitetext" name="invitetext" value=documentinvitetext />
              </div>
              <div style="margin-top: 20px">
              <span>
              <input type="checkbox" class="addremovecheckbox flashOnClick" rel="#daystosignbox" location="#datetosigncontainer" oldlocation="#hiddenttimestuffbox" autocomplete="off" value=(if timetosignset then "on" else "off") ></input> Välj förfallodatum
                <div id="datetosigncontainer">  </div>
                <% if timetosignset
                   then <span/>
                   else <span class="hidden flashMessage" > Varning: Om du väljer ett förfallodatum kan du inte återkalla inbjudan innan datumet förfallit. Detta regleras av avtalslagen.</span>
                   
                %>   
              </span>
              <div style="height: 2px;"/>
              <input class="bigbutton cross-button" type="submit" name="final" value="Underteckna" id="signinvite" rel="#dialog-confirm-signinvite"/> <br />
              <input class="button" type="submit" name="save" value="Spara som utkast"/>
              </div>
              </form>
              <span class="localdialogs">
                <form method="post" name="form" action=(LinkIssueDoc documentid) class="overlay redirectsubmitform" id="dialog-confirm-signinvite" rel="#main-document-form">  
                   <a class="close"> </a>
                   <h2 id="dialog-title-sign">Underteckna</h2>
                   <h2 id="dialog-title-send">Skicka inbjudan</h2>
                   <div id="dialog-confirm-text-sign">
                    <p>Är du säker att du vill underteckna dokumentet <strong><% documenttitle %></strong>?</p>
                    
                    <p>När du undertecknat kommer en automatisk inbjudan att skickas till 
                                    
                    <span class="Xinvited">Invited</span> med e-post.</p>
                   </div>

                   <div id="dialog-confirm-text-send">
                     <p>Du har valt en sekreterarroll och kommer själv inte att underteckna. Är du säker på att du vill skicka en inbjudan att underteckna dokumentet <strong><% documenttitle %></strong> till <span class="Xinvited">Invited</span>?</p>
                   </div>
                   
                   <div class="buttonbox" >
                       <input type="hidden" name="final" value="automatic"/>
                       <button class="close button" type="button"> Avbryt </button>
                       <button class="submiter button" type="button"> Underteckna </button>
                       </div>
                 </form>  
                 <form method="post" name="form" action="" class="overlay" id="edit-invite-text-dialog" >  
                   <a class="close"> </a>
                   <h2>Hälsningsmeddelande</h2>
                   <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
                   <% fmap cdata $ mailInvitationToSignContent (ctxtemplates ctx) False ctx document author Nothing%>
                   </div>
                   <div class="buttonbox" >
                       <button class="close button" type="button"> Avbryt </button>
                       <button class="editer button" type=""> Skriv eget meddelande </button>
                       <button class="close button" type="button" id="editing-invite-text-finished"> Ok </button>
                   </div>
                 </form>  
                 <div class="hidden" id="hiddenttimestuffbox">
                       <div id="daystosignbox">Undertecknas inom (dagar)
                        <BR/>
                        <input type="text" id="daystosign" name="daystosign" value=documentdaystosignboxvalue maxlength="2" size="2" autocomplete="off"/>
                        <small> <a  class="datetodaystip" rel="#daystosign"> </a> </small>
                       </div>
                 </div>
               </span>
             </span>
           else
               <span>
               <% if documentstatus == Pending || documentstatus == AwaitingAuthor 
                   then
                      <script type="text/javascript" language="Javascript" src="/js/showfields.js">  </script>
                   else <span /> %>
               <% if ((documentstatus == Pending || documentstatus == AwaitingAuthor) &&  anyInvitationUndelivered document)
                          then <p> Adressen 
                                   <strong>
                                    <% BS.intercalate (BS.fromString ", ") $ map (signatoryemail . signatorydetails) $ undeliveredSignatoryLinks document %> 
                                   </strong> existerar inte. Kontrollera adressen och försök igen.
                               </p>
                          else <span/>
               %>       
               <script type="text/javascript">
                 <% "var docstate = " ++ (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks) ++ ";" %>
               </script>
               
              <div id="signatorylist">
                 <% fmap (cdata . concat) $ sequence $ map (showSignatoryLinkForSign' ctx document author) documentsignatorylinks
                 %>
              </div>
              <% if documentstatus == AwaitingAuthor
                  then <form method="post" action=""><input class="bigbutton cross-button" type="submit" name="final" value="Underteckna" id="signinvite" /></form>
                  else <span />%>
              </span>
              %>
            <% if (documentstatus == Pending || documentstatus == AwaitingAuthor) 
                then 
                   if not timetosignset
                    then <span>
                     <input class="button cancel" type="button" name="cancel" value="Återkalla inbjudan"  rel="#cancel-by-author-dialog" />    
                     <span class="localdialogs">
                     <form method="post" action=(LinkCancel document) class="overlay" id="cancel-by-author-dialog">
                                <a class="close"> </a>
                                <h2> Återkalla inbjudan </h2>
                                <p>Är du säker att du vill återkalla din inbjudan att underteckna dokumentet?
                                <BR/>När du återkallat inbjudan kommer nedanstaende meddelande att skickas till dina motparter.
                                </p>
                                <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
                                 <% fmap cdata $ mailCancelDocumentByAuthorContent  (ctxtemplates ctx) False Nothing ctx document author%>
                                </div>
                                <div class="buttonbox" >
                                   <button class="close button" type="button"> Avbryt </button>
                                   <button class="editer button" type=""> Skriv eget meddelande </button>
                                   <button class="submiter button" type="button"> Återkalla inbjudan</button>
                                </div>
                          </form>
                       </span>
                       </span>
                    else <span>Du kan inte återkalla inbjudan före förfallodatum.</span>
                else <span/>
             %>         
            <% fmap cdata $
               if (documentstatus == Canceled || documentstatus == Timedout || documentstatus == Rejected || documentstatus == Withdrawn)
               then renderActionButton  (ctxtemplates ctx) (LinkRestart documentid) "restartButtonName"
               else return ""
             %>  
       </div>
      </div>

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
                              } 
             author
             =
   let
       allinvited = documentsignatorylinks
       documentauthordetails = signatoryDetailsFromUser author
   in do
      signatoryentryforedit <- showSignatoryEntryForEdit2 (ctxtemplates ctx) "signatory_template" "" "" "" ""
      helpers <- renderTemplate (ctxtemplates ctx) "pageDocumentForViewerHelpers" [("documentid",show documentid),
                                                                                   ("signatoryentryforedit",signatoryentryforedit)]                          
      let localscript = "var docstate = " ++ (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks) ++ ";" 
      signatorylist <- fmap concat $ sequence $ map (showSignatoryLinkForSign' ctx document author) allinvited
      content <- renderTemplate (ctxtemplates ctx) "pageDocumentForViewerContent" [("localscript",localscript),
                                                                                   ("signatorylist",signatorylist)]                          
      showDocumentPageHelper' (ctxtemplates ctx) document helpers (documenttitle) content


showDocumentPageHelper' templates document helpers title content =
   do 
   docbox <- showDocumentBox templates  
   renderTemplateComplex templates "showDocumentPageHelper" $  
                                                              (setAttribute "helpers" $ helpers) .
                                                              (setAttribute "docbox" $ docbox) .
                                                              (setAttribute "title" $ title) .
                                                              (setAttribute "content" $ content ) .
                                                              (setAttribute "linkissuedocpdf" $ show (LinkIssueDocPDF document)) 


showDocumentPageHelper templates document helpers title content =
    <div class="docview">
     <div style="display: none">
      <% helpers %>
     </div>
      <div class="docviewleft">
       <% fmap cdata $ showDocumentBox templates%>
      </div>
      <div class="docviewright"> 
       <p><strong><% title %></strong><br/>
          <small><a href=(LinkIssueDocPDF document) target="_blank">Ladda ned PDF</a></small></p>
       <% content %>
      </div>
     <div class="clearboth"/>
    </div> 

showSignatoryLinkForSign' ctx@(Context {ctxmaybeuser = muser,ctxtemplates})  document author siglnk@(SignatoryLink{  signatorylinkid 
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
      let wasSigned =  isJust maybesigninfo
      let wasSeen = isJust maybeseeninfo
      let isTimedout = documentstatus document == Timedout
      let isCanceled = documentstatus document == Canceled
      let isRejected = documentstatus document == Rejected
      let isWithDrawn = documentstatus document == Withdrawn
      let dontShowAnyReminder = isTimedout || isCanceled || isRejected || isWithDrawn
      let isCurrentUserAuthor = maybe False (isAuthor document) muser
      let isCurrentSignatorAuthor = (fmap (unEmail . useremail . userinfo) muser) ==  (Just signatoryemail)                  
      let dialogHeight =   if (wasSigned) then "400" else "600"
      let status =  caseOf
                [
                ( invitationdeliverystatus == Undelivered,"/theme/images/status_rejected.png"),
                ( isWithDrawn,"/theme/images/status_rejected.png"), 
                ( isCanceled, "/theme/images/status_rejected.png"), 
                ( isRejected, "/theme/images/status_rejected.png"), 
                ( isTimedout, "/theme/images/status_timeout.png"), 
                ( wasSigned,  "/theme/images/status_signed.png"),
                ( wasSeen,    "/theme/images/status_viewed.png" )]        
                              "/theme/images/status_pending.png"
      message <- caseOf
                   [
                    (wasSigned, renderTemplate ctxtemplates "signatoryMessageSigned" [("date", showDateOnly $ signtime $ fromJust maybesigninfo)]),  
                    (isTimedout, renderTemplate ctxtemplates "signatoryMessageTimedout" []),
                    (isCanceled || isRejected || isWithDrawn, return "" ),
                    (wasSeen,  renderTemplate ctxtemplates "signatoryMessageSeen" [("date", showDateOnly $ signtime $ fromJust maybeseeninfo)])
                   ]        (renderTemplate ctxtemplates "signatoryMessageNotSigned" [])
      reminderText <- if (wasSigned)
                      then renderTemplate ctxtemplates "reminderTextSigned" [] 
                      else renderTemplate ctxtemplates "reminderTextNotSigned" []
      reminderSenderText <- 
                     if (wasSigned)
                      then renderTemplate ctxtemplates "reminderSenderTextSigned" [] 
                      else renderTemplate ctxtemplates "reminderSenderTextNotSigned" []             
      reminderEditorText <- renderTemplate ctxtemplates "reminderEditorText" []                         
      reminderDialogTitle <- return reminderText   
      reminderMessage <-  mailDocumentRemindContent ctxtemplates Nothing ctx document siglnk author
      reminderForm <- whenMaybe (isCurrentUserAuthor && (not isCurrentSignatorAuthor) && (not dontShowAnyReminder) && (invitationdeliverystatus /= Undelivered)) $
                        renderTemplateComplex ctxtemplates "reminderForm" $  
                                         (setAttribute "reminderDialogTitle" $  reminderDialogTitle) .
                                         (setAttribute "signatorylinkid" $  show signatorylinkid ) .
                                         (setAttribute "reminderText" $  reminderDialogTitle) .
                                         (setAttribute "reminderMessage" $ reminderMessage ) .
                                         (setAttribute "reminderEditorText" $ reminderEditorText) .
                                         (setAttribute "reminderSenderText" $ reminderSenderText ) .
                                         (setAttribute "dialogHeight" $ dialogHeight ) .
                                         (setAttribute "linkremind" $ show (LinkRemind document siglnk)) 
                                      
      changeEmailAddress <-  whenMaybe (isCurrentUserAuthor && (invitationdeliverystatus == Undelivered) && (not dontShowAnyReminder)) $ 
                                renderTemplateComplex ctxtemplates "changeEmailAddress" $  
                                         (setAttribute "linkchangeemail" $  show $ LinkChangeSignatoryEmail (documentid document) signatorylinkid) .
                                         (setAttribute "signatoryemail" $  BS.toString signatoryemail)          
      renderTemplateComplex ctxtemplates "showSignatoryLinkForSign" $  
                              (setAttribute "mainclass" $         if isCurrentSignatorAuthor  then "author" else "signatory") .
                              (setAttribute "status" $ status) .
                              (setAttribute "signatoryname" $     packToMString signatoryname ) .
                              (setAttribute "signatorycompany" $ packToMString signatorycompany) .
                              (setAttribute "signatorynumber" $   packToMString signatorynumber) .
                              (setAttribute "signatoryemail" $    packToMString signatoryemail) .
                              (setAttribute "fields" $ signatoryotherfields ) .
                              (setAttribute "message" $  message) .
                              (setAttribute "reminderForm" $ reminderForm) .
                              (setAttribute "changeEmailAddress" $  changeEmailAddress) 
                       

packToMString x = if BS.null x then Nothing else (Just x) 

pageDocumentForSign :: KontraLink 
                    -> Document 
                    -> Context
                    -> SignatoryLink
                    -> Bool 
                    -> User
                    -> (HSPT IO XML) 
pageDocumentForSign action document ctx  invitedlink wassigned author =
   let helpers = [ <script> var documentid = "<% show $ documentid document %>"; 
                  </script>
                , <script type="text/javascript">
                   <% "var docstate = " ++ (buildJS documentauthordetails $ map signatorydetails (documentsignatorylinks document)) ++ "; docstate['useremail'] = '" ++ (BS.toString $ signatoryemail $ signatorydetails invitedlink) ++ "';" %>
                  </script>
                , <script src="/js/signatory.js" /> ]
       magichash = signatorymagichash invitedlink
       authorname = signatoryname documentauthordetails
       allbutinvited = {- filter (/= invitedlink) -} (documentsignatorylinks document)
       documentauthordetails = signatoryDetailsFromUser author
       rejectMessage =  fmap cdata $ mailRejectMailContent (ctxtemplates ctx) Nothing ctx (prettyName author) document (personname invitedlink)
   in showDocumentPageHelper (ctxtemplates ctx) document helpers
              (documenttitle document) $
              <span>
                 <p>Vänligen var noga med att granska dokumentet och kontrollera 
                    uppgifterna nedan innan du undertecknar.</p>   

                 <% fmap (cdata . concat) $ sequence $ map (showSignatoryLinkForSign' ctx document author) (allbutinvited) %>
                 <% caseOf 
                    [(wassigned ,
                              <div>Du har redan undertecknat!</div>),
                    (documentstatus document == Timedout, 
                              <div>Förfallodatum har passerat!</div>),
                    (documentstatus document == Pending, 
                              <div>
                                 <input class="bigbutton" type="submit" name="sign" value="Underteckna" id="sign" rel="#dialog-confirm-sign"/>
                                 <input class="bigbutton" type="submit" name="cancel" value="Avvisa" rel="#dialog-confirm-cancel" id="cancel"/>
                              </div>) ]
                     <span/>                 
                 %>
                 {- <small>Jag vill veta mer <a href="/about" target="_blank">om SkrivaPå</a>.</small> -}
                 <span class="localdialogs ">
                  <form method="post" name="form" action=action id="dialog-confirm-sign" class="overlay">     
                     <a class="close"> </a>                  
                     <h2>Underteckna</h2>  
                     <p>Är du säker att du vill underteckna dokumentet <strong><% documenttitle document %></strong>?</p>
                     <p>När <% partyUnsignedMeAndListString magichash document %> undertecknat blir 
                      avtalet <strong>juridiskt bindande</strong> och
                      det färdigställda avtalet skickas till din e-post.</p>
                      <div class="buttonbox">
                      <input type="hidden" name="sign" value="automatic"/>
                      <button class="close button" type="button"> Avbryt </button>
                      <button class="submiter button" type="button"> Underteckna </button>
                      </div>
                  </form>
                 <form method="post" name="form" action=action id="dialog-confirm-cancel" class="overlay">   
                    <a class="close"> </a>     
                       <h2>Avvisa</h2>                 
                    <p>Är du säker på att du vill avvisa dokumentet <strong><% documenttitle document %></strong>?</p>
                    <p>När du avvisat kommer vi att skicka ett e-postmeddelande för att meddela <strong><% authorname %></strong>.</p>
                    <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
                     <% rejectMessage %>
                    </div>
                    <div class="buttonbox">
                     <input type="hidden" name="cancel" value="automatic"/>
                     <button class="close button" type="button"> Avbryt </button>
                     <button class="editer button" type="button"> Skriv eget meddelande </button>
                     <button class="submiter button" type="button"> Avvisa </button>
                    </div> 
                 </form> 
                 </span>
                 
              </span>
     



--We keep this javascript code generation for now
jsArray :: [[Char]] -> [Char]
jsArray xs = "[" ++ (joinWith ", " xs) ++ "]"

buildDefJS :: FieldDefinition -> [Char]
buildDefJS (FieldDefinition { fieldlabel, fieldvalue, fieldplacements }) = 
    "{ label: " ++ show fieldlabel -- show because we need quotes
                    ++ ", value: " ++ show fieldvalue
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
    "{ name: " ++ show signatoryname
                   ++ ", company: " ++ show signatorycompany
                   ++ ", email: " ++ show signatoryemail
                   ++ ", number: " ++ show signatorynumber
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
