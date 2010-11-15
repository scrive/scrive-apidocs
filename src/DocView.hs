{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module DocView( emptyDetails
              , showFilesImages2
              , pageDocumentForAuthor
              , pageDocumentList
              , mailInvitationToSign
              , mailDocumentClosedForSignatories
              , mailDocumentClosedForAuthor
              , landpageSignInviteView
              , landpageSignedView
              , landpageLoginForSaveView
              , landpageDocumentSavedView
              , pageDocumentForSign
              , flashDocumentDraftSaved
              , remindMail
              , flashRemindMailSent
              , mailDocumentRejectedForAuthor
              , mailDocumentAwaitingForAuthor
              , landpageRejectedView
              , flashDocumentRejected
              , defaultInviteMessage 
              ) where
import AppView
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
import "mtl" Control.Monad.Trans
import DocViewMail
import DocViewUtil
    
instance Monad m => IsAttrValue m DocumentID where
    toAttrValue = toAttrValue . show


landpageSignInviteView :: (XMLGenerator m) => 
                          Context -> 
                          Document -> 
                          XMLGenT m (HSX.XML m)
landpageSignInviteView ctx document@Document{ documenttitle
                                            , documentsignatorylinks
                                            } =
    <div class="centerdivnarrow">
     <p class="headline">Dokumentet <strong><% documenttitle %></strong> undertecknat!</p>
     
     <p>En inbjudan att underteckna har nu skickats 
        till <% partyListButAuthorString document %>.
     </p>

     <p><a class="button" href="/">Skapa ett nytt avtal</a></p>
    </div>


{-

Variables:
1. all signed?
2. has account already?
3. is logged in as the account?

Let as skip 3 for now.
-}


{-

   Fulldoc + account
   Halfdoc + account
   Fulldoc + no account
   Halfdoc + no account

-}

willCreateAccountForYou ctx _ _ False = 
    <p>Dokumentet har sparats till ditt konto. 
     <% loginBox ctx %>
    </p>
willCreateAccountForYou ctx document siglink True = 
    <p>Du kan nu spara dokumentet på SkrivaPå, då är ditt dokument säkert lagrat och dessutom 
       kan du även i framtiden verifiera avtalet mot vår databas. Detta kostar ingenting. 
      <form action=(LinkLandpageSaved document siglink) method="post">
       <input class="button" type="submit" value="Skapa konto"/>
      </form>
    </p>

landpageRejectedView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink),EmbedAsAttr m (Attr [Char] BS.ByteString)) => 
                      Context -> 
                      Document -> 
                      SignatoryLink -> 
                      Bool ->
                      XMLGenT m (HSX.XML m)
landpageRejectedView ctx document@Document{documenttitle,documentstatus} signatorylink hasaccount =
    <div class="centerdivnarrow">
      <p class="headline">Du har avvisat dokumentet <strong><% documenttitle %></strong>.</p>
      <p>Ett meddelande har skickats till <% partyListString document %>.</p>
      <p><a href="/">Tillbaks till förstasidan</a></p>
    </div>


landpageSignedView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink),EmbedAsAttr m (Attr [Char] BS.ByteString)) => 
                      Context -> 
                      Document -> 
                      SignatoryLink -> 
                      Bool ->
                      XMLGenT m (HSX.XML m)
landpageSignedView ctx document@Document{documenttitle,documentstatus} signatorylink hasaccount 
    | documentstatus == Closed =
    <div class="centerdivnarrow">
      <p class="headline">Dokumentet är färdigställt</p>
      <p>Du har undertecknat dokumentet <strong><% documenttitle %></strong>. Således har 
         <% partyListString document %> undertecknat dokumentet och avtalet är nu juridiskt bindande.</p>
      <% willCreateAccountForYou ctx document signatorylink (not hasaccount) %>
    </div>

landpageSignedView ctx document@Document{documenttitle} signatorylink hasaccount =
    <div class="centerdivnarrow">
      <p class="headline">Du har undertecknat</p>
      <p>Du har nu undertecknat dokumentet <strong><% documenttitle %></strong>. 
         <% partyUnsignedListString document %> har ännu inte undertecknat 
         dokumentet. När alla undertecknat blir avtalet juridiskt bindande och
         en kopia av det färdigställda dokumentet skickats då till din e-post.</p>
      <% willCreateAccountForYou ctx document signatorylink (not hasaccount) %>
    </div>

landpageLoginForSaveView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
                         => Context -> XMLGenT m (HSX.XML m)
landpageLoginForSaveView ctx  =
    <div class="centerdivnarrow">
     <a class="headline">Login</a>
     <p>Ditt dokument är nu sparat. Du finner dokumentet under Avtal when you log in.</p>
     <% loginBox ctx %>
    </div>


landpageDocumentSavedView :: (XMLGenerator m) =>  XMLGenT m (HSX.XML m)
landpageDocumentSavedView = 
    <div class="centerdivnarrow">
     <p>Ditt dokument är nu sparat. Du finner dokumentet under Arkiv när du loggar in.</p>
 
     <p>Vi hoppas att du är nöjd med vår tjänst hittills och att du är nyfiken på att själv använda 
        SkrivaPå för att skriva dina avtal. Därför erbjuder vi dig som ny kund möjligheten att testa 
        tjänsten genom tre fria avtal. Dina fria avtal förbrukas endast då ett avtal undertecknats av 
        alla parter.</p>

     <p>Börja redan nu! Ladda upp ditt avtal genom att klicka nedan.</p>
     <a class="bigbutton" href="/">Starta</a> {- FIXME: move upload stuff here also -}
    </div>

flashDocumentDraftSaved :: HSP.HSP HSP.XML
flashDocumentDraftSaved  = 
    <div>
     Du har sparat documentet.
    </div>


concatSignatories :: [SignatoryDetails] -> String
concatSignatories siglinks = 
    concat $ intersperse ", " $ map (BS.toString . signatoryname) siglinks 

oneDocumentRow::(EmbedAsAttr m (Attr [Char] [Char]), EmbedAsAttr m (Attr [Char] KontraLink), EmbedAsAttr m (Attr [Char] DocumentID)) =>
                MinutesTime -> UserID -> Document  -> XMLGenT m (HSX.XML m)
oneDocumentRow crtime userid document@Document{ documentid
                                , documentsignatorylinks
                                , documentstatus
                                , documenttitle
                                , documenttimeouttime
                                , documentmtime
                                , documentauthor
                                }  = 
    let link = if unAuthor documentauthor==userid
               then LinkIssueDoc document
               else LinkSignDoc document signatorylink
        [signatorylink] = filter (\x -> maybesignatory x == Just (Signatory userid)) documentsignatorylinks
        mk x = <a href=link><% x %></a>
        seenstatus = any (isJust . maybeseeninfo) documentsignatorylinks
        statusimg = "/theme/images/" ++
                    case documentstatus of
                      Preparation -> "status_draft.png"
                      Pending  -> if seenstatus
                                  then "status_viewed.png"
                                  else "status_pending.png"
                      -- question: what status icon to use?
                      AwaitingAuthor -> "status_signed.png"
                      Closed -> "status_signed.png"
                      Canceled -> "status_rejected.png"
                      Timedout -> "status_timeout.png"
                      Rejected -> "status_rejected.png"
                      AwaitingAuthor ->  "status_pending.png" --TODO: use bettter image
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
     <td><% mk $ concatSignatories (map signatorydetails documentsignatorylinks) %></td>
     <td><% mk $ documenttitle %></td>
     <td><span title=(show documentmtime)><% mk $ showDateAbbrev crtime documentmtime %></span></td>
     <td class="tdright"></td>
    </tr>


pageDocumentList :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink),
                      EmbedAsAttr m (Attr [Char] DocumentID)) 
              => MinutesTime
              -> UserID
              -> [Document] 
              -> XMLGenT m (HSX.XML m)
pageDocumentList ctime userid documents = 
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
       <% map (oneDocumentRow ctime userid) (filter (not . documentdeleted) documents) %>
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
    <div id=idx>
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

pageDocumentForAuthor :: (Monad m) 
             => Context 
             -> Document 
             -> (HSPT m XML) 
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
              <form method="post" name="form" action=(LinkIssueDoc document) id="main-document-form"> 
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
              <p>Undertecknas inom (dagar)
              <input type="text" name="daystosign" value=documentdaystosign maxlength="2" size="2" autocomplete="off"/>
              </p>
              
              <div style="height: 2px;"/>
              <input class="bigbutton" type="submit" name="final" value="Underteckna" id="signinvite" rel="#dialog-confirm-signinvite"/>
              <input class="button" type="submit" name="save" value="Spara som utkast"/>
              </div>
              </form>
              <span class="localdialogs">
                <form method="post" name="form" action=(LinkIssueDoc document) class="overlay redirectsubmitform" id="dialog-confirm-signinvite" rel="#main-document-form">  
                   <a class="close"> </a>
                   <h2>Underteckna</h2>
                    <p>Är du säker att du vill underteckna dokumentet <strong><% documenttitle %></strong>?</p>
                    
                    <p>När du undertecknat kommer en automatisk inbjudan att skickas till 
                       <span class="Xinvited">Invited</span> med e-post.</p>
                   <BR/>    
                   <BR/>
                   <BR/>
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
                   <% mailInvitationToSignContent False ctx document Nothing%>
                   </div>
                   <BR/>
                   <BR/>
                   <div class="buttonbox" >
                       <button class="close button" type="button"> Avbryt </button>
                       <button class="editer button" type=""> Skriv eget meddelande </button>
                       <button class="close button" type="button" id="editing-invite-text-finished"> Ok </button>
                   </div>
                 </form>  
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

showSignatoryLinkForSign :: (Monad m) => Context -> Document -> SignatoryLink -> GenChildList (HSPT' m)
showSignatoryLinkForSign ctx@(Context {ctxmaybeuser = muser})  document siglnk@(SignatoryLink{  signatorylinkid 
                                       , maybesigninfo
                                       , maybeseeninfo
                                       , signatorydetails = SignatoryDetails
                                                            { signatoryname
                                                            , signatorynumber
                                                            , signatorycompany
                                                            , signatoryemail
                                                            }
                                         }) =
   let
      wasSigned =  isJust maybesigninfo
      wasSeen = isJust maybeseeninfo
      isTimedout = documentstatus document == Timedout
      
      status =  caseOf
                [( isTimedout, <img src="/theme/images/status_timeout.png"/>), 
                (wasSigned, <img src="/theme/images/status_signed.png"/>),
                (wasSeen, <img src="/theme/images/status_viewed.png"/> )]
                <img src="/theme/images/status_pending.png"/>
      message = caseOf
                [
                (wasSigned, "Undertecknat " ++ showDateOnly (signtime $ fromJust maybesigninfo) ),
                (isTimedout, "Förfallodatum har passerat"),
                (wasSeen,  "Granskat " ++ showDateOnly (signtime $ fromJust maybeseeninfo))]
                "Har ej undertecknat"       
      isCurrentUserAuthor = (isJust muser) && isAuthor (fromJust muser) document    
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
      reminderMessage =  remindMailContent Nothing ctx document siglnk
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
   in asChild <div class=(if isCurrentUserAuthor then "author" else "signatory")><% 
                [asChild status,asChild " "] ++
                (if BS.null signatoryname then [] else [ asChild <strong><% signatoryname %></strong>, asChild <br/> ]) ++
                (if BS.null signatorycompany then [] else [ asChild signatorycompany, asChild <br/> ]) ++
                (if BS.null signatorynumber then [] else [ asChild signatorynumber, asChild <br/> ]) ++
                (if BS.null signatoryemail then [] else [ asChild signatoryemail, asChild <br/> ]) ++
                [asChild <span class="signatoryfields" />] ++
                ([asChild message]) ++
                (if (isCurrentUserAuthor && (not isCurrentSignatorAuthor) && (not isTimedout)) then [asChild <br/> ,asChild reminderForm] else [])
                %>
              </div>

pageDocumentForSign :: ( Monad m) =>
                       KontraLink 
                           -> Document 
                           -> Context
                           -> SignatoryLink
                           -> Bool 
                    -> (HSPT m XML) 
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
       rejectMessage =  rejectMailContent Nothing ctx (personname authorlink) document (personname invitedlink)
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
                              <div>Förfallodatum har passerat!</div>)
                    ]
                              <div>
                                 <input class="bigbutton" type="submit" name="sign" value="Underteckna" id="sign" rel="#dialog-confirm-sign"/>
                                 <input class="bigbutton" type="submit" name="cancel" value="Avvisa" rel="#dialog-confirm-cancel" id="cancel"/>
                              </div>
                 %>
                 <small>Jag vill veta mer <a href="/about" target="_blank">om SkrivaPå</a>.</small>
                 <span class="localdialogs ">
                  <form method="post" name="form" action=action id="dialog-confirm-sign" class="overlay">     
                     <a class="close"> </a>                  
                     <h2>Underteckna</h2>  
                     <p>Är du säker att du vill underteckna dokumentet <strong><% documenttitle document %></strong>?</p>
                     <p>När <% partyUnsignedMeAndListString magichash document %> undertecknat blir 
                      avtalet <strong>juridiskt bindande</strong> och
                      det färdigställda avtalet skickas till din e-post.</p>
                      <BR/>
                      <BR/>
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

flashDocumentRejected :: Document -> HSP.HSP HSP.XML
flashDocumentRejected document@Document{ documenttitle } = 
    <div>
     Du har avvisat dokumentet <strong><% documenttitle %></strong>.
     Ett meddelande har skickats till <% partyListString document %>.
    </div>
     
     
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
                                     
flashRemindMailSent :: SignatoryLink -> BS.ByteString                                    
flashRemindMailSent signlink =  
                case signlink of 
                  SignatoryLink{maybesigninfo = Nothing} -> (BS.fromString "En påminnelse har skickats till ") `BS.append` (personname signlink)
                  _ -> (BS.fromString "Dokumentet har skickats till ") `BS.append` (personname signlink)

defaultInviteMessage :: BS.ByteString
defaultInviteMessage = BS.empty                              