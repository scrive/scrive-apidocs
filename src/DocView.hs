{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}

module DocView( emptyDetails
              , showFilesImages2
              , pageDocumentForAuthor
              , pageDocumentList
              , landpageSignInviteView
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
              , mailDocumentClosedForSignatories
              , mailDocumentClosedForAuthor
              
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
import Templates    

landpageSignInviteView :: Document ->  IO String
landpageSignInviteView document =
     do 
      partylist <-renderListTemplate (map (BS.toString . personname') $ partyListButAuthor document)
      renderTemplate "landpageSignInviteView" [("partyListButAuthor", partylist),
                                               ("documenttitle",BS.toString $ documenttitle document )]

willCreateAccountForYou:: Document->SignatoryLink->Bool->  IO String
willCreateAccountForYou _ _ False =  renderTemplate "willCreateAccountForYouNoAccount" ([]::[(String,String)])
willCreateAccountForYou document siglink True = 
                                     renderTemplate "willCreateAccountForYouHasAccount" [("documentid",show $ unDocumentID $ documentid document),
                                                                                         ("signatorylinkid",show $ unSignatoryLinkID $ signatorylinkid siglink)]

landpageRejectedView :: Document -> IO String
landpageRejectedView document =
   do 
      partylist <-renderListTemplate (map (BS.toString . personname') $ partyList document)
      renderTemplate "landpageRejectedView" [("partyList", partylist),
                                              ("documenttitle",BS.toString $ documenttitle document )]

landpageSignedView :: Document -> SignatoryLink -> Bool -> IO String
landpageSignedView document@Document{documenttitle,documentstatus} signatorylink hasaccount =
    do
       willCreateAccountForYouProposal <- willCreateAccountForYou document signatorylink (not hasaccount) 
       if (documentstatus == Closed) 
        then do
              partylist <- renderListTemplate $ map (BS.toString . personname') $ partyList document
              renderTemplate "landpageSignedViewClosed" [("partyListString", partylist),
                                                         ("documenttitle",BS.toString $ documenttitle),
                                                         ("willCreateAccountForYou", willCreateAccountForYouProposal)]
        else do
              partyunsignedlist <- renderListTemplate $ map (BS.toString . personname') $ partyUnsignedList document
              renderTemplate "landpageSignedViewNotClosed"  [("partyUnsignedListString", partyunsignedlist),
                                                             ("documenttitle",BS.toString $ documenttitle),
                                                             ("willCreateAccountForYou", willCreateAccountForYouProposal)]   

landpageLoginForSaveView::IO String
landpageLoginForSaveView  = renderTemplate "landpageLoginForSaveView" []

landpageDocumentSavedView :: IO String
landpageDocumentSavedView = renderTemplate "landpageDocumentSavedView" []

flashDocumentDraftSaved :: IO String
flashDocumentDraftSaved  = renderTemplate "flashDocumentDraftSaved" []

flashDocumentRestarted :: IO String
flashDocumentRestarted  = renderTemplate "flashDocumentRestarted" []

flashRemindMailSent :: SignatoryLink -> IO String                                
flashRemindMailSent  signlink@SignatoryLink{maybesigninfo = Nothing}  = renderTemplate "flashRemindMailSentNotSigned" [("personname",BS.toString $ personname signlink)] 
flashRemindMailSent  signlink = renderTemplate "flashRemindMailSentSigned" [("personname",BS.toString $ personname signlink)] 


flashMessageCanceled :: IO String
flashMessageCanceled = renderTemplate "flashMessageCanceled" []

concatSignatories :: [SignatoryLink] -> String
concatSignatories siglinks = 
    concat $ intersperse ", " $ map (BS.toString . personname) siglinks

oneDocumentRow::(EmbedAsAttr m (Attr [Char] [Char]), EmbedAsAttr m (Attr [Char] KontraLink), EmbedAsAttr m (Attr [Char] DocumentID)) =>
                MinutesTime -> User -> Document  -> XMLGenT m (HSX.XML m)
oneDocumentRow crtime user document@Document{ documentid
                                , documentsignatorylinks
                                , documentstatus
                                , documenttitle
                                , documenttimeouttime
                                , documentmtime
                                , documentauthor
                                }  = 
    let link = if unAuthor documentauthor==(userid user)
               then LinkIssueDoc documentid
               else LinkSignDoc document signatorylink
        [signatorylink] = filter (isMatchingSignatoryLink user) documentsignatorylinks
        mk x = <a href=link><% x %></a>
        seenstatus = any (isJust . maybeseeninfo) documentsignatorylinks
        statusimg = "/theme/images/" ++
                    case documentstatus of
                      Preparation -> "status_draft.png"
                      Pending  -> if seenstatus
                                  then "status_viewed.png"
                                  else "status_pending.png"
                      -- question: what status icon to use?
                      AwaitingAuthor -> "status_pending.png"
                      Closed -> "status_signed.png"
                      Canceled -> "status_rejected.png"
                      Timedout -> "status_timeout.png"
                      Rejected -> "status_rejected.png"

        dateDiffInDays (MinutesTime ctime) (MinutesTime mtime)
                       | ctime>mtime = 0
                       | otherwise = (mtime - ctime) `div` (60*24)
    in
    <tr class="ui-state-default">
     <td class="tdleft">
      <input type="checkbox" name="doccheck" value=documentid class="check" />
     </td>
     <td><img width="17" height="17" src=statusimg/></td>
     <td>
      <% case documenttimeouttime of
                   Nothing -> <span/>
                   -- FIXME: show days to sign, not the final date
                   Just (TimeoutTime x) -> 
                       if documentstatus==Pending
                       then <span title=("Förfallodatum: " ++ show x)><% mk $ "(" ++ show (dateDiffInDays crtime x) ++ ")" %></span>
                       else <span/>
       %>
     </td>
     <td><% mk $ concatSignatories documentsignatorylinks %></td>
     <td><% mk $ documenttitle %></td>
     <td><span title=(show documentmtime)><% mk $ showDateAbbrev crtime documentmtime %></span></td>
     <td class="tdright"></td>
    </tr>


pageDocumentList :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink),
                      EmbedAsAttr m (Attr [Char] DocumentID)) 
              => MinutesTime
              -> User
              -> [Document] 
              -> XMLGenT m (HSX.XML m)
pageDocumentList ctime user documents = 
     <form method="post" action=LinkIssue>
     <table class="doctable" cellspacing="0">
      <col/>
      <col/>
      <col/>
      <col/>
      <col/>
      <col/>
      <thead>
       <tr>
        <td><a href="#" id="all">Alla</a></td>
        <td></td> {- status icon -}
        <td></td> {- Förfallodatum -}
        <td>Motparter</td>
        <td>Dokument</td>
        <td>Senaste handelse</td>
        <td></td>
       </tr>
      </thead>
      <tfoot>
       <tr>
        <td colspan="7" style="text-align: right; overflow: hidden;">
          <div class="floatleft">
           <input type="submit" class="button" name="archive" value="Radera"/>
          </div>
          <div class="floatright">
           <img src="/theme/images/status_draft.png"/> Utkast
           <img src="/theme/images/status_rejected.png"/> Avbrutet
           <img src="/theme/images/status_timeout.png"/> Förfallet
           <img src="/theme/images/status_pending.png"/> Skickat
           <img src="/theme/images/status_viewed.png"/> Granskat av motpart
           <img src="/theme/images/status_signed.png"/> Undertecknat
          </div>
          <div class="clearboth"/>
         </td>
       </tr>
      </tfoot>
      <tbody id="selectable">
       <% map (oneDocumentRow ctime user) (filter (not . documentdeleted) documents) %>
      </tbody>
     </table>
     </form>

showSignatoryEntryForEdit :: ( XMLGenerator m, EmbedAsAttr m (Attr [Char] KontraLink),
                               EmbedAsAttr m (Attr [Char] DocumentID)) 
                          => DocState.SignatoryDetails -> XMLGenT m (HSX.XML m)
showSignatoryEntryForEdit (SignatoryDetails{signatoryname,signatorycompany,signatorynumber, signatoryemail}) = 
    showSignatoryEntryForEdit2 "" (BS.toString signatoryname) 
                                   (BS.toString signatorycompany) 
                                   (BS.toString signatorynumber) 
                                   (BS.toString signatoryemail)

showSignatoryEntryForEdit2 :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink),
                               EmbedAsAttr m (Attr [Char] DocumentID)) 
                           => String -> String -> String -> String
                           -> String -> XMLGenT m (HSX.XML m)
showSignatoryEntryForEdit2 idx signatoryname signatorycompany signatorynumber signatoryemail = 
    <div id=idx class="signatorybox" alt="Namn på avtalspart">
      <input name="signatoryname" type="text" value=signatoryname autocomplete="off"
             infotext="Namn på motpart"/><br/>
      <input name="signatorycompany" type="text" value=signatorycompany autocomplete="off"
             infotext="Titel, företag"/><br/>
      <input name="signatorynumber" type="text" value=signatorynumber autocomplete="off"
             infotext="Orgnr/Persnr"/><br/>
      <input name="signatoryemail"  type="email" value=signatoryemail autocomplete="off"
             infotext="Personens e-mail"/><br/>
      <small><a onclick="return signatoryremove(this.parentNode);" href="#">Ta bort</a></small>
    </div>

    
showFileImages ::(EmbedAsAttr m (Attr [Char] [Char])) => File -> JpegPages -> [XMLGenT m (HSX.XML m)]    
showFileImages File{fileid} (JpegPages jpgpages) =
   [ <div id=("page" ++ show pageno) class="pagediv"><img class="pagejpg" src=("/pages/" ++ show fileid ++ "/" ++ show pageno) width="300" /></div> |
     pageno <- [1..(length jpgpages)]]
     
showFileImages _ JpegPagesPending = 
   [ <div class="pagejpga4 pagejpg">
      <img class="waiting" src="/theme/images/wait30trans.gif"/>
     </div> ]
     
showFileImages _ (JpegPagesError normalizelog) = 
   [ <div class="pagejpga4 pagejpg">
      <% normalizelog %>
     </div> ]
     
showFilesImages2 :: (EmbedAsAttr m (Attr [Char] [Char])) => [(File, JpegPages)] -> XMLGenT m (HSX.XML m)
showFilesImages2 files = <span><% concatMap (uncurry showFileImages) files %></span> 

showDocumentBox :: (EmbedAsAttr m (Attr [Char] [Char])) => XMLGenT m (HSX.XML m)
showDocumentBox = 
    <div id="documentBox">
     <div class="pagejpga4 pagejpg">
      <img class="waiting" src="/theme/images/wait30trans.gif"/>
     </div>
    </div> 

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

pageDocumentForAuthor :: Context 
             -> Document 
             -> (HSPT IO XML) 
pageDocumentForAuthor ctx
             document@Document{ documentsignatorylinks
                              , documentauthordetails
                              , documenttitle
                              , documentid
                              , documentstatus
                              , documentdaystosign
                              , documentinvitetext
                              } 
             =
   let helper = [ showSignatoryEntryForEdit2 "signatory_template" "" "" "" ""
                , <script> var documentid = "<% show $ documentid %>"; 
                  </script>
                ]
       allinvited = documentsignatorylinks
       authorlink = SignatoryLink 
                    { signatorydetails = documentauthordetails
                    , maybeseeninfo = Nothing 
                      -- this gymanstic below is to cover up for some earlier error
                      -- that erased sign times of authors
                    , maybesigninfo = if isJust (documentmaybesigninfo document)
                                      then (documentmaybesigninfo document)
                                      else if documentstatus == Closed
                                           then Just (SignInfo (MinutesTime 0) 0)
                                           else Nothing
                    , signatorylinkid = SignatoryLinkID 0
                    , maybesignatory = Nothing -- FIXME: should be author user id
                    , signatorymagichash = MagicHash 0
                    }
       documentdaystosignboxvalue = maybe 7 id documentdaystosign
       timetosignset = isJust documentdaystosign --swedish low constrain
   in showDocumentPageHelper document helper 
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
              <strong><% signatoryname documentauthordetails %></strong><br/>
              <% addbr $ signatorycompany documentauthordetails %>
              <% addbr $ signatorynumber documentauthordetails %>
              <% addbr $ signatoryemail documentauthordetails %>
              </div>
              Motpart<br/>
              <div id="signatorylist">
               <% map showSignatoryEntryForEdit (if null documentsignatorylinks
                                                 then [emptyDetails] 
                                                 else map signatorydetails documentsignatorylinks) %>
              </div>
              <small><a id="addsiglink" onclick="signatoryadd(); return false;" href="#">Lägg till fler</a></small>
              <div style="margin-top: 5px">
              <small><a rel="#edit-invite-text-dialog" id="editinvitetextlink" href="#" style="padding-top:3px">Hälsningsmeddelande</a></small>
              <input type="hidden" id="invitetext" name="invitetext" value=documentinvitetext />
              </div>
              <div style="margin-top: 5px">
              <span>
              <input type="checkbox" class="addremovecheckbox flashOnClick" rel="#daystosignbox" location="#datetosigncontainer" oldlocation="#hiddenttimestuffbox" autocomplete="off" value=(if timetosignset then "on" else "off") ></input> Välj förfallodatum
                <div id="datetosigncontainer">  </div>
                <% if timetosignset
                   then <span/>
                   else <span class="hidden flashMessage" > Varning: Om du väljer ett förfallodatum kan du inte återkalla inbjudan innan datumet förfallit. Detta regleras av avtalslagen.</span>
                   
                %>   
              </span>
              <div style="height: 2px;"/>
              <input class="bigbutton" type="submit" name="final" value="Underteckna" id="signinvite" rel="#dialog-confirm-signinvite"/>
              <input class="button" type="submit" name="save" value="Spara som utkast"/>
              </div>
              </form>
              <span class="localdialogs">
                <form method="post" name="form" action=(LinkIssueDoc documentid) class="overlay redirectsubmitform" id="dialog-confirm-signinvite" rel="#main-document-form">  
                   <a class="close"> </a>
                   <h2>Underteckna</h2>
                    <p>Är du säker att du vill underteckna dokumentet <strong><% documenttitle %></strong>?</p>
                    
                    <p>När du undertecknat kommer en automatisk inbjudan att skickas till 
                       <span class="Xinvited">Invited</span> med e-post.</p>
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
                   <% fmap cdata $ mailInvitationToSignContent False ctx document Nothing%>
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
               <% if documentstatus == Pending || documentstatus == AwaitingAuthor then
                      <script type="text/javascript" language="Javascript" src="/js/showfields.js"> 
                                  </script>
                  else <span /> %>
               <script type="text/javascript">
                 <% "var docstate = " ++ (buildJS documentauthordetails $ map signatorydetails documentsignatorylinks) ++ ";" %>
               </script>
               
              <div id="signatorylist">
                 <% showSignatoryLinkForSign ctx document authorlink %>
                 <% map (showSignatoryLinkForSign ctx document) allinvited 
                 %>
              </div>
              <% if documentstatus == AwaitingAuthor
                  then <form method="post" action=""><input class="bigbutton" type="submit" name="final" value="Underteckna" id="signinvite" /></form>
                  else <span />%>
              </span>
              %>
            <% if (documentstatus == Pending || documentstatus == AwaitingAuthor) && not timetosignset
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
                                 <% fmap cdata $ mailCancelDocumentByAuthorContent False Nothing ctx document%>
                                </div>
                                <div class="buttonbox" >
                                   <button class="close button" type="button"> Avbryt </button>
                                   <button class="editer button" type=""> Skriv eget meddelande </button>
                                   <button class="submiter button" type="button"> Återkalla inbjudan</button>
                                </div>
                          </form>
                       </span>
                       </span>

                  else <span />
             %>         
            <% fmap cdata $
               if (documentstatus == Canceled || documentstatus == Timedout || documentstatus == Rejected)
               then renderActionButton (LinkRestart documentid) "restartButtonName"
               else return ""
             %>  
       </div>
      </div>

showDocumentPageHelper
    :: (XMLGenerator m, 
        HSX.EmbedAsChild m c,
        EmbedAsAttr m (Attr [Char] KontraLink),
        HSX.EmbedAsChild m d,
        EmbedAsAttr m (Attr [Char] BS.ByteString)) =>
        DocState.Document
     -> c
     -> BS.ByteString
     -> d
     -> XMLGenT m (HSX.XML m)
showDocumentPageHelper document helpers title content =
    <div class="docview">
     <div style="display: none">
      <% helpers %>
     </div>
      <div class="docviewleft">
       <% showDocumentBox%>
      </div>
      <div class="docviewright"> 
       <p><strong><% title %></strong><br/>
          <small><a href=(LinkIssueDocPDF document) target="_blank">Ladda ned PDF</a></small></p>
       <% content %>
      </div>
     <div class="clearboth"/>
    </div> 

showSignatoryLinkForSign :: Context -> Document -> SignatoryLink -> GenChildList (HSPT' IO)
showSignatoryLinkForSign ctx@(Context {ctxmaybeuser = muser})  document siglnk@(SignatoryLink{  signatorylinkid 
                                       , maybesigninfo
                                       , maybeseeninfo
                                       , signatorydetails = SignatoryDetails
                                                            { signatoryname
                                                            , signatorynumber
                                                            , signatorycompany
                                                            , signatoryemail
                                                            , signatoryotherfields
                                                            }
                                         }) =
   let
      wasSigned =  isJust maybesigninfo
      wasSeen = isJust maybeseeninfo
      isTimedout = documentstatus document == Timedout
      isCanceled = documentstatus document == Canceled
      isRejected = documentstatus document == Rejected
      dontShowAnyReminder = isTimedout || isCanceled 
      status =  caseOf
                [
                ( isCanceled, <img src="/theme/images/status_rejected.png"/>), 
                ( isRejected, <img src="/theme/images/status_rejected.png"/>), 
                ( isTimedout, <img src="/theme/images/status_timeout.png"/>), 
                (wasSigned, <img src="/theme/images/status_signed.png"/>),
                (wasSeen, <img src="/theme/images/status_viewed.png"/> )]
                <img src="/theme/images/status_pending.png"/>
      message = caseOf
                [
                (wasSigned, "Undertecknat " ++ showDateOnly (signtime $ fromJust maybesigninfo) ),
                (isTimedout, "Förfallodatum har passerat"),
                (isCanceled, "" ),
                (isRejected, "" ),
                (wasSeen,  "Granskat " ++ showDateOnly (signtime $ fromJust maybeseeninfo))]
                 "Har ej undertecknat"       
      isCurrentUserAuthor = maybe False (isAuthor document) muser
      isCurrentSignatorAuthor = (fmap (unEmail . useremail) muser) ==  (Just signatoryemail)    
      reminderText = if (wasSigned)
                      then "Skicka dokumentet igen"
                      else "Skicka påminnelse"
      reminderSenderText = 
                     if (wasSigned)
                      then "Skicka"
                      else "Skicka påminnelse"               
      reminderEditorText = "Skriv eget meddelande"                          
      reminderDialogTitle = reminderText
      reminderMessage =  fmap cdata $  mailDocumentRemindContent Nothing ctx document siglnk
      dialogHeight =   if (wasSigned) then "400" else "600"
      reminderForm = <span>
                      <a style="cursor:pointer" class="prepareToSendReminderMail" rel=("#siglnk" ++ (show signatorylinkid ))>  <% reminderText %>  </a>
                      <form class="overlay" action=(LinkRemind document siglnk) method="POST" title=reminderDialogTitle width="600" height=dialogHeight id=("siglnk" ++ (show signatorylinkid))>
                       <a class="close"> </a>
                       <h2> <% reminderDialogTitle %> </h2>
                       <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
                         <% reminderMessage %>
                       </div>
                       <div class="buttonbox">
                       <button class="close button" type="button"> Avbryt </button>
                       <button class="editer button" type="button"> <%reminderEditorText%> </button>
                       <button class="submiter button" type="button"> <%reminderSenderText%> </button>
                       </div>
                      </form>     
                    </span>  
   in asChild <div class=(if isCurrentSignatorAuthor then "author" else "signatory")><% 
                [asChild status,asChild " "] ++
                (if BS.null signatoryname then [] else [ asChild <strong><% signatoryname %></strong>, asChild <br/> ]) ++
                (if BS.null signatorycompany then [] else [ asChild signatorycompany, asChild <br/> ]) ++
                (if BS.null signatorynumber then [] else [ asChild signatorynumber, asChild <br/> ]) ++
                (if BS.null signatoryemail then [] else [ asChild signatoryemail, asChild <br/> ]) ++
                [asChild <div class="signatoryfields"><% map displayField signatoryotherfields %></div>] ++
                ([asChild message]) ++
                (if (isCurrentUserAuthor && (not isCurrentSignatorAuthor) && (not dontShowAnyReminder)) then [asChild <br/> ,asChild reminderForm] else [])
                %>
              </div>

displayField::(Monad m) => FieldDefinition -> (HSPT m XML) 
displayField FieldDefinition {fieldlabel, fieldvalue} 
    | fieldvalue == BS.fromString "" = <span />
    | otherwise        = <div><span class="fieldlabel"><% fieldlabel %>: </span><span class="fieldvalue"><% fieldvalue %></span></div>

pageDocumentForSign ::  KontraLink 
                           -> Document 
                           -> Context
                           -> SignatoryLink
                           -> Bool 
                    -> (HSPT IO XML) 
pageDocumentForSign action document ctx  invitedlink wassigned =
   let helpers = [ <script> var documentid = "<% show $ documentid document %>"; 
                  </script>
                , <script type="text/javascript">
                   <% "var docstate = " ++ (buildJS (documentauthordetails document) $ map signatorydetails (documentsignatorylinks document)) ++ "; docstate['useremail'] = '" ++ (BS.toString $ signatoryemail $ signatorydetails invitedlink) ++ "';" %>
                  </script>
                , <script src="/js/signatory.js" /> ]
       magichash = signatorymagichash invitedlink
       authordetails = documentauthordetails document
       authorname = signatoryname authordetails
       allbutinvited = filter (/= invitedlink) (documentsignatorylinks document)
       authorlink = SignatoryLink 
                    { signatorydetails = authordetails
                    , maybeseeninfo = Nothing 
                      -- this gymanstic below is to cover up for some earlier error
                      -- that erased sign times of authors
                    , maybesigninfo = if isJust (documentmaybesigninfo document)
                                      then (documentmaybesigninfo document)
                                      else if documentstatus document == Closed
                                           then Just (SignInfo (MinutesTime 0) 0)
                                           else Nothing
                    , signatorylinkid = SignatoryLinkID 0
                    , maybesignatory = Nothing -- FIXME: should be author user id
                    , signatorymagichash = MagicHash 0
                    }
       rejectMessage =  fmap cdata $ mailRejectMailContent Nothing ctx (personname authorlink) document (personname invitedlink)
   in showDocumentPageHelper document helpers
              (documenttitle document) $
              <span>
                 <p>Vänligen var noga med att granska dokumentet och kontrollera 
                    uppgifterna nedan innan du undertecknar.</p>   

                 <% showSignatoryLinkForSign ctx document invitedlink %>

                 <% map (showSignatoryLinkForSign ctx document) (authorlink : allbutinvited) %>
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
