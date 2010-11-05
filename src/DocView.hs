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
              , landpageRejectedView
              , flashDocumentRejected
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
import SendMail(Mail,emptyMail,content,title,attachments)
import InspectXML
import UserView ()
import "mtl" Control.Monad.Trans

$(deriveInspectXML ''Document)

$(deriveInspectXML ''FieldDefinition)

$(deriveInspectXML ''FieldPlacement)

instance InspectXML DocumentID where
    inspectXML x = asChild <a href=("/dave/document/" ++ show x)><% show x %></a>

instance InspectXML SignatoryLinkID where
    inspectXML x = asChild <a href=("/dave/signatorylink/" ++ show x)><% show x %></a>
    
instance InspectXML Author where
    inspectXML (Author x) = inspectXML x

$(deriveInspectXML ''SignatoryLink)

instance InspectXML File where
    inspectXML = asChild . show
instance InspectXML DocumentStatus where
    inspectXML = asChild . show
instance InspectXML ChargeMode where
    inspectXML = asChild . show
instance InspectXML TimeoutTime where
    inspectXML = asChild . show

$(deriveInspectXML ''SignatoryDetails)

instance InspectXML SignInfo where
    inspectXML = asChild . show
instance InspectXML DocumentHistoryEntry where
    inspectXML = asChild . show


instance InspectXML MagicHash where
    inspectXML = asChild . show
instance InspectXML Signatory where
    inspectXML = asChild . show
    
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
        <table>
         <tr><td>Lösenord:</td>
             <td><input type="password" name="password"/></td>
         </tr>
         <tr><td>Upprepa lösenord:</td>
             <td><input type="password" name="password2"/></td>
         </tr>
         <tr><td colspan="2">
             <input type="checkbox" name="tos" id="tos">Jag har läst och accepterar 
                    <a href="/termsofuse.html">SkrivaPå Allmänna Villkor</a></input>
             </td>
         </tr>
       </table>
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
                         => Context -> Document -> SignatoryLink -> XMLGenT m (HSX.XML m)
landpageLoginForSaveView ctx document signatorylink =
    {- let loginlink = maybeSignInLink2 ctx "Login" (LinkLandpageSaved document signatorylink) 
                    "bigbutton" in
     -}
    <div class="centerdivnarrow">
     <a class="headline">Login</a>
     <p>Ditt dokument är nu sparat. Du finner dokumentet under Avtal when you log in.</p>
        {- <p>För att du ska kunna komma åt ditt avtal i framtiden skapar vi ett konto till dig.</p>
         -}
     <% loginBox ctx %>
    </div>


landpageDocumentSavedView :: (XMLGenerator m) => Context -> Document -> SignatoryLink -> XMLGenT m (HSX.XML m)
landpageDocumentSavedView (ctx@Context { ctxmaybeuser = Just user }) document signatorylink = 
    <div class="centerdivnarrow">
     <p>Ditt dokument är nu sparat. Du finner dokumentet under Arkiv när du loggar in.</p>
 
     <p>Vi hoppas att du är nöjd med vår tjänst hittills och att du är nyfiken på att själv använda 
        SkrivaPå för att skriva dina avtal. Därför erbjuder vi dig som ny kund möjligheten att testa 
        tjänsten genom tre fria avtal. Dina fria avtal förbrukas endast då ett avtal undertecknats av 
        alla parter.</p>

     <p>Börja redan nu! Ladda upp ditt avtal genom att klicka nedan.</p>
     <a class="bigbutton" href="/">Starta</a> {- FIXME: move upload stuff here also -}
    </div>


flashDocumentIssued :: Document -> HSP.HSP HSP.XML
flashDocumentIssued document = 
    <div>
     Du har undertecknat avtalet och en inbjudan har nu skickats 
     till <% partyListButAuthorString document %>.
    </div>

flashDocumentDraftSaved :: Document -> HSP.HSP HSP.XML
flashDocumentDraftSaved document = 
    <div>
     Du har sparat documentet.
    </div>

flashDocumentSigned :: Document -> HSP.HSP HSP.XML
flashDocumentSigned document =
    <div>
     Du har undertecknat avtalet!
    </div>

flashDocumentClosed :: Document -> HSP.HSP HSP.XML
flashDocumentClosed document =
    <div>
     Du har undertecknat avtalet! Avtalet är undertecknat av <% partyListString document %> nu!
    </div>
  
mkSignDocLink :: String -> Document -> SignatoryLink -> String
mkSignDocLink hostpart documentid signaturelink =
    hostpart ++ show (LinkSignDoc documentid signaturelink)


concatSignatories :: [SignatoryDetails] -> String
concatSignatories siglinks = 
    concat $ intersperse ", " $ map (BS.toString . signatoryname) siglinks 

-- oneDocumentRow :: (XMLGenerator m) => MinutesTime -> UserID -> Document -> GenChild m
oneDocumentRow ctime userid document@Document{ documentid
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
                       then <span title=("Förfallodatum: " ++ show x)><% mk $ "(" ++ show (dateDiffInDays ctime x) ++ ")" %></span>
                       else <span/>
       %>
     </td>
     <td><% mk $ concatSignatories (map signatorydetails documentsignatorylinks) %></td>
     <td><% mk $ documenttitle %></td>
     {- 
     <td><% mk $ case documenttimeouttime of
                   Nothing -> "-"
                   -- FIXME: show days to sign, not the final date
                   Just (TimeoutTime x) -> 
                       if documentstatus==Pending || documentstatus==Timedout  
                       then showDateAbbrev ctime x
                       else "-"
          %>
     </td>
     -}
     <td><span title=(show documentmtime)><% mk $ showDateAbbrev ctime documentmtime %></span></td>
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

showFile
  :: (EmbedAsChild m String) =>
     File -> XMLGenT m (HSX.XML m)
showFile file = <li><% show file %></li>

showSignatory
  :: (EmbedAsChild m String, Show a) => a -> XMLGenT m (HSX.XML m)
showSignatory sig = <li><% show sig %></li>



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

    
    
showFileImages File{fileid} (JpegPages jpgpages) =
   [ <div id=("page" ++ show pageno) class="pagediv"><img class="pagejpg" src=("/pages/" ++ show fileid ++ "/" ++ show pageno) width="300" /></div> |
     pageno <- [1..(length jpgpages)]]
showFileImages File{fileid} JpegPagesPending = 
   [ <div class="pagejpga4 pagejpg">
      <img class="waiting" src="/theme/images/wait30trans.gif"/>
     </div> ]
showFileImages File{fileid} (JpegPagesError normalizelog) = 
   [ <div class="pagejpga4 pagejpg">
      <% normalizelog %>
     </div> ]

showFilesImages2 files = <span><% concatMap (uncurry showFileImages) files %></span> 

showDocumentBox document = 
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

addbr text | BS.null text = []
addbr text = [<% text %>, <% <br/> %>]

pageDocumentForAuthor :: (Monad m) 
             => Context 
             -> Document 
             -> Bool 
             -> Int                -- free documents left
             -> (HSPT m XML) 
pageDocumentForAuthor ctx@(Context {ctxmaybeuser = Just user})
             document@Document{ documentsignatorylinks
                              , documentauthordetails
                              , documenttitle
                              , documentid
                              , documentstatus
                              , documentdaystosign
                              , documentinvitetext
                              } 
             issuedone 
             freeleft =
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

        <% if documentstatus == Preparation
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

              <div style="margin-top: 10px">
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
                   <div style="padding-left:5px">
                   <% makeEditable "invitetext" $ withCustom  (Just documentinvitetext) <p></p> %>
                   </div>
                   <BR/>
                   <BR/>
                   <div class="buttonbox" >
                       <input type="hidden" name="final" value="automatic"/>
                       <button class="submiter" type="button"> Underteckna </button>
                       <button class="editer" type="button"> Skriv eget meddelande   </button>
                       <button class="close" type="button"> Avbryt </button>
                   </div>
                 </form>  
               </span>
             </span>
           else
              <div id="signatorylist">
                 <% showSignatoryLinkForSign ctx document authorlink %>
                 <% map (showSignatoryLinkForSign ctx document) allinvited 
                 %>
              </div>
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
       <% showDocumentBox document %>
      </div>
      <div class="docviewright"> 
       <p><strong><% title %></strong><br/>
          <small><a href=(LinkIssueDocPDF document) target="_blank">Ladda ned PDF</a></small></p>
       <% content %>
      </div>
     <div class="clearboth"/>
    </div> 

class ( XMLGenerator m
      , EmbedAsAttr m (Attr [Char] KontraLink)
      , EmbedAsAttr m (Attr [Char] BS.ByteString)) => XMLGenerator2 m


-- cover for a bug that we did forget time of signing
showDateOnly1 (MinutesTime 0) = ""
showDateOnly1 x = showDateOnly x

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
                (wasSigned, "Undertecknat " ++ showDateOnly1 (signtime $ fromJust maybesigninfo) ),
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
      mainDialogText =  if (wasSigned)
                         then <span>Du vill skicka dokumentet <% documenttitle document %> till <%(personname siglnk)%> igen. Nedan ser du vårt standardmeddelande. Om du vill kan du ändra meddelandet Innan du skickar.</span>
                         else <span>Du vill skicka en påminnelse att underteckna dokumentet <% documenttitle document %> till <%(personname siglnk)%>. Nedan ser du vårt standardmeddelande. Om du vill kan du ändra meddelandet Innan du skickar.</span>
      dialogHeight =   if (wasSigned) then "400" else "600"
      reminderForm = <span>
                      <a style="cursor:pointer" class="prepareToSendReminderMail" rel=("#siglnk" ++ (show signatorylinkid ))>  <% reminderText %>  </a>
                      <form class="overlay" action=(LinkRemind document siglnk) method="POST" title=reminderDialogTitle width="600" height=dialogHeight id=("siglnk" ++ (show signatorylinkid))>
                       <a class="close"> </a>
                       <h2> <% reminderDialogTitle %> </h2>
                       <p>
                         <% mainDialogText %>
                       </p>  
                       <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
                         <% reminderMessage %>
                       </div>
                       <div class="buttonbox">
                       <button class="submiter" type="button"> <%reminderSenderText%> </button>
                       <button class="editer" type="button"> <%reminderEditorText%> </button>
                       <button class="close" type="button"> Avbryt </button>
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
                %></div>

pageDocumentForSign :: ( Monad m) =>
                       KontraLink 
                           -> Document 
                           -> Context
                           -> SignatoryLink
                           -> Bool 
                    -> (HSPT m XML) 
pageDocumentForSign action document ctx@(Context {ctxmaybeuser = muser})  invitedlink wassigned =
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
       rejectMessage =  rejectMailContent Nothing ctx BS.empty (personname authorlink) document (personname invitedlink)
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
                      <button class="submiter" type="button"> Underteckna </button>
                      <button class="close" type="button"> Avbryt </button>
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
                     <button class="submiter" type="button"> Avvisa </button>
                     <button class="editer" type="button"> Skriv eget meddelande </button>
                     <button class="close" type="button"> Avbryt </button>
                    </div> 
                 </form> 
                 </span>
                 
              </span>



mailInvitationToSign :: Context
               -> BS.ByteString
               -> BS.ByteString
               -> Document
               -> SignatoryLink
               -> MagicHash
               -> IO Mail
mailInvitationToSign (Context {ctxmaybeuser = Just user, ctxhostpart}) 
                  emailaddress personname 
                  document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails,documentinvitetext} 
                  signaturelink magichash = 
    let link = ctxhostpart ++ show (LinkSignDoc document signaturelink)
        creatorname = signatoryname documentauthordetails
        --common :: (XMLGenerator m) => GenChildList m
        common = sequence
              [ asChild <p><i>Översiktlig information</i><br/>
                 Parter: <% partyListString document %><br/>
                 Har undertecknat: <strong><% creatorname %></strong><br/>
                <% case documenttimeouttime of 
                     Just time -> <span>Undertecknas senast: <strong><% show time %></strong>.</span>
                     Nothing -> <span/>
                 %> 
                 </p>
              , asChild <p><i>Så här går du vidare</i><br/>
              1. Klicka på länken<br/>
              <a href=link><% link %></a><br/>
              2. Granska online<br/>
              3. Underteckna<br/>
              4. Färdig<br/>
              </p>
             ]
        --skrivapaversion  :: (XMLGenerator m) => GenChildList m
        skrivapaversion = sequence 
            [ asChild <p>Hej <strong><% personname %></strong>,</p>
            , asChild <p><strong><% creatorname %></strong> har bjudit in dig att underteckna dokumentet 
               <strong><% documenttitle %></strong> online via tjänsten SkrivaPå.</p> 
            , asChild common
            , asChild (poweredBySkrivaPaPara ctxhostpart)
            ]

        --authorversion  :: (XMLGenerator m) => GenChildList m
        authorversion = sequence $

               [ <% cdata $ BS.toString documentinvitetext %>
               , asChild common
               , asChild "Hälsningar"
               , asChild <br/>
               , asChild creatorname
               , asChild <br/>
               ]
        content = if BS.null documentinvitetext
                    then <span><% skrivapaversion %></span>
                    else <span><% authorversion %></span>
    in 
       do
        let title =  BS.concat [BS.fromString "Inbjudan från ",creatorname , BS.fromString " till ", documenttitle]
        content <- htmlHeadBodyWrapIO documenttitle content
        return $ emptyMail {title = title, content = content}

mailDocumentClosedForSignatories :: Context
           -> BS.ByteString
           -> BS.ByteString
           -> Document
           -> SignatoryLink
           -> MagicHash
           -> IO Mail
mailDocumentClosedForSignatories (Context {ctxhostpart}) 
              emailaddress personname 
              document@Document{documenttitle,documentid} 
              signaturelink magichash = do
    let title = BS.append (BS.fromString "Bekräftelse: ")  documenttitle    
    let link = ctxhostpart ++ show (LinkSignDoc document signaturelink)
    content <- htmlHeadBodyWrapIO documenttitle 
         <span>
           <p>Hej <strong><% personname %></strong>,</p>
           <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats 
             av <% partyListString document %>. Avtalet är nu bindande.</p> 
          
           <p>Det färdigställda dokumentet bifogas med detta mail.</p> 
   
           <% poweredBySkrivaPaPara ctxhostpart %>
         </span>
    return $ emptyMail {title = title, content = content}

mailDocumentClosedForAuthor :: Context
                 -> BS.ByteString
                 -> BS.ByteString
                 -> Document
                 -> IO Mail
mailDocumentClosedForAuthor (Context {ctxhostpart}) 
                  emailaddress personname 
                  document@Document{documenttitle,documentid} = 
    do
     let title = BS.append (BS.fromString "Bekräftelse: ")  documenttitle
     let link = ctxhostpart ++ show (LinkIssueDoc document)
     content <- htmlHeadBodyWrapIO documenttitle
        <span>
          <p>Hej <strong><% personname %></strong>,</p>
             <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats 
                av <% partyListString document %>. Avtalet är nu bindande.</p> 

             <p>Det färdigställda dokumentet bifogas med detta mail.</p> 

             <% poweredBySkrivaPaPara ctxhostpart %>
        </span> 
     return $ emptyMail {title = title, content = content}

flashDocumentRejected :: Document -> HSP.HSP HSP.XML
flashDocumentRejected document@Document{ documenttitle } = 
    <div>
     Du har avvisat dokumentet <strong><% documenttitle %></strong>.
     Ett meddelande har skickats till <% partyListString document %>.
    </div>

mailDocumentRejectedForAuthor :: (Maybe BS.ByteString) -> Context
                   -> BS.ByteString
                   -> BS.ByteString
                   -> Document
                   -> BS.ByteString
                   -> IO Mail
mailDocumentRejectedForAuthor customMessage 
                   ctx@(Context {ctxhostpart}) 
                   emailaddress personname 
                   document@Document{documenttitle,documentid} 
                           rejectorName = do
     let title = BS.append (BS.fromString "Avvisat: ")  documenttitle
     content <- htmlHeadBodyWrapIO documenttitle $ rejectMailContent customMessage ctx emailaddress personname document rejectorName
     return $ emptyMail {title = title, content = content}

rejectMailContent customMessage (Context {ctxhostpart}) 
                   emailaddress personname 
                   document@Document{documenttitle,documentid} 
                   rejectorName =   
     let link = ctxhostpart ++ show (LinkIssueDoc document)
         content = 
          <span>
            <p>Hej <% personname %>,</p>
            <p><% rejectorName %> har nekat att underteckna dokumentet <strong><% documenttitle %></strong>. 
               Avtalsprocessen är därmed avbruten.</p>
               <% poweredBySkrivaPaPara ctxhostpart %>
          </span>  
     in makeEditable "customtext" $ withCustom customMessage content    
     

partyList :: Document -> [SignatoryDetails]
partyList document =
    let author = documentauthordetails document
        signas = map signatorydetails (documentsignatorylinks document)
    in author : signas

partyUnsignedList :: Document -> [SignatoryDetails]
partyUnsignedList document =
    let signalinks = documentsignatorylinks document
        unsignalinks = filter ((== Nothing) . maybesigninfo) signalinks
        signas = map signatorydetails unsignalinks
    in signas

partyUnsignedMeAndList :: MagicHash -> Document -> [SignatoryDetails]
partyUnsignedMeAndList magichash document =
    let signalinks = documentsignatorylinks document
        cond signlink = signatorymagichash signlink /= magichash &&
                        maybesigninfo signlink == Nothing
        unsignalinks = filter cond signalinks
        me = SignatoryDetails { signatoryname = BS.fromString "du"
                              , signatorycompany = BS.empty
                              , signatorynumber = BS.empty
                              , signatoryemail = BS.empty
                              , signatorynameplacements = []
                              , signatorycompanyplacements = []
                              , signatorynumberplacements = []
                              , signatoryemailplacements = []
                              , signatoryotherfields = []
                              }
        signas = map signatorydetails unsignalinks
    in me : signas

partyListButAuthor :: Document -> [SignatoryDetails]
partyListButAuthor document =
    map signatorydetails (documentsignatorylinks document)

partyListButAuthorString :: (XMLGenerator m) => Document -> GenChildList m
partyListButAuthorString document =
    swedishListString (map (strong . BS.toString . signatoryname) (partyListButAuthor document))

{-
partyListButAuthor :: Document -> [SignatoryDetails]
partyListButAuthor document =
    map signatorydetails (documentsignatorylinks document)
-}  

strong :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
strong x = <strong><% x %></strong>

partyListString :: (XMLGenerator m) => Document -> GenChildList m
partyListString document = 
    swedishListString (map (strong . BS.toString . signatoryname) (partyList document))

partyUnsignedListString :: (XMLGenerator m) => Document -> GenChildList m
partyUnsignedListString document = 
    swedishListString (map (strong . BS.toString . signatoryname) (partyUnsignedList document))

partyUnsignedMeAndListString :: (XMLGenerator m) => MagicHash -> Document -> GenChildList m
partyUnsignedMeAndListString magichash document =
    swedishListString (map (strong . BS.toString . signatoryname) (partyUnsignedMeAndList magichash document))


swedishListString :: (XMLGenerator m) => [XMLGenT m (HSX.XML m)] -> GenChildList m
swedishListString [] = return []
swedishListString [x] = asChild x
swedishListString [x, y] = do
  list <- sequence [asChild x, asChild " och ", asChild y]
  return (concat list)
swedishListString (x:xs) = do
  list <- sequence [asChild x, asChild ", "]
  list2 <- swedishListString xs
  return (concat list ++ list2)

remindMail:: Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
remindMail cm c d s = case s of 
                       SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSigned cm c d s
                       _ -> remindMailSigned cm c d s
                       
remindMailContent :: (Monad m) => Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink ->(HSPT m XML) 
remindMailContent cm c d s = case s of 
                               SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSignedContent True cm c d s
                               _ -> remindMailSignedContent True cm c d s
                       
remindMailNotSigned::Maybe (BS.ByteString) -> Context -> Document -> SignatoryLink -> IO Mail
remindMailNotSigned customMessage ctx@Context{ctxmaybeuser = Just user, ctxhostpart}
           document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails,documentinvitetext} 
           signlink = 
    let link = ctxhostpart ++ show (LinkSignDoc document signlink)
        content  = remindMailNotSignedContent False customMessage ctx  document signlink                 
        attachmentcontent = filepdf $ head $ documentfiles document          
        title =  BS.concat [BS.fromString "Hej ",personname signlink]  
      in 
       do
        content <- htmlHeadBodyWrapIO documenttitle content
        return $ emptyMail {title = title, content = content}

        
        
remindMailSigned::Maybe (BS.ByteString)-> Context -> Document -> SignatoryLink -> IO Mail
remindMailSigned customMessage ctx@Context{ctxmaybeuser = Just user, ctxhostpart}
                 document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails,documentinvitetext} 
                 signlink = 
                     let 
                         title =  BS.concat [BS.fromString "Hej ",personname signlink]              
                         attachmentcontent = filepdf $ head $ documentfiles document   
                         content = remindMailSignedContent False customMessage ctx  document signlink                             
                     in do
                       content <- htmlHeadBodyWrapIO documenttitle content
                       return $ emptyMail {title = title, content = content, attachments = [(documenttitle,attachmentcontent)]}

remindMailNotSignedContent::(Monad m) => Bool ->  (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> (HSPT m XML)                                       
remindMailNotSignedContent hideSensitive customMessage ctx  document signlink =  
               let link =  (ctxhostpart ctx) ++ show (LinkSignDoc document signlink)
                   link' =  if (hideSensitive)
                             then <a href="#"> https://skrivapa.se/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/</a>
                             else <a href=link><% link %></a>
                   common = sequence
                            [ asChild <p><i>Översiktlig information</i><br/>
                              Parter: <% partyListString document %><br/>
                              Har undertecknat: <strong><% signatoryname $ documentauthordetails document %></strong><br/>
                              <% case (documenttimeouttime document) of 
                                 Just time -> <span>Undertecknas senast: <strong><% show time %></strong>.</span>
                                 Nothing -> <span/>
                              %> 
                             </p>
                             , asChild <p><i>Så här går du vidare</i><br/>
                             1. Klicka på länken<br/>
                              <% link' %><br/>
                             2. Granska online<br/>
                             3. Underteckna<br/>
                             4. Färdig<br/>
                             </p>
                            ]
                   skrivapaversion = 
                        <span> 
                          <%common%>
                          <%(poweredBySkrivaPaPara (ctxhostpart ctx)) %>
                        </span>          
                   header   = withCustom  customMessage (remindMailNotSignedStandardHeader ctx document signlink)
                   in (makeEditable "customtext" header) `before` skrivapaversion       

remindMailSignedContent ::(Monad m) => Bool -> (Maybe BS.ByteString) -> Context -> Document -> SignatoryLink -> (HSPT m XML)                  
remindMailSignedContent hideSensitive customMessage ctx  document signlink        
                         = makeEditable "customtext" $
                            withCustom 
                                customMessage $
                                (remindMailSignedStandardHeader ctx document signlink) `before` (poweredBySkrivaPaPara (ctxhostpart ctx))                     
                                      
                                      
                                      
remindMailSignedStandardHeader::(Monad m) => Context -> Document -> SignatoryLink -> (HSPT m XML) 
remindMailSignedStandardHeader ctx@Context{ctxmaybeuser = Just user} document signlink =        
                                               <span>
                                                 <p>Hej <%(personname signlink)%><%"\n"%><%"\n"%></p>

												<p><%(userfullname user)%> har begärt att vi ska skicka dokumentet <% documenttitle document %> till dig som du har undertecknat via tjänsten SkrivaPå. Dokumentet bifogas med detta mail.<%"\n"%></p>
                                                </span>
                       
remindMailNotSignedStandardHeader::(Monad m) => Context -> Document -> SignatoryLink -> (HSPT m XML) 
remindMailNotSignedStandardHeader ctx@Context{ctxmaybeuser = Just user} document signlink =   
                                                <span>
                                                 <p>Hej <%(personname signlink)%><%"\n"%><%"\n"%></p>
                                                 
                                                 <p><%(signatoryname $ documentauthordetails document)%> vill påminna dig om att du inte undertecknat dokument <% documenttitle document %> ännu.<%"\n"%><%"\n"%></p>  
                                               </span>
                                                
withCustom::(Monad m) => Maybe (BS.ByteString)->(HSPT m XML) ->(HSPT m XML) 

withCustom (Just customMessage) _ =  <p> <% (cdata $ BS.toString customMessage ) %> </p>
withCustom Nothing standardMessage = standardMessage       

                                                        
before::(Monad m) => (HSPT m XML)-> (HSPT m XML)-> (HSPT m XML) 
before header message = <p><span><%header%></span><span><%message%></span> </p>                                     
                                           
makeEditable::(Monad m) => String -> (HSPT m XML) ->(HSPT m XML) 
makeEditable name c = <div class="editable" name=name><%c%></div>         
                                  
personname signlink = if (BS.null $ signatoryname $ signatorydetails signlink)
                        then  signatoryemail $ signatorydetails signlink  
                        else  signatoryname $ signatorydetails signlink
              
joinWith _ [] = []
joinWith _ [x] = x
joinWith s (x:xs) = x ++ s ++ (joinWith s xs)

jsArray xs = "[" ++ (joinWith ", " xs) ++ "]"

buildDefJS fd@FieldDefinition { fieldlabel, fieldvalue, fieldplacements } = 
    "{ label: " ++ show fieldlabel -- show because we need quotes
                    ++ ", value: " ++ show fieldvalue
                    ++ ", placements: " ++ (jsArray (map buildPlacementJS fieldplacements))
                    ++ " }"

buildPlacementJS pl@FieldPlacement { placementx, placementy, placementpage, placementpagewidth, placementpageheight } = 
    "{ x: " ++ show placementx 
                ++ ", y: " ++ show placementy
                ++ ", page: " ++ show placementpage
                ++ ", h: " ++ show placementpageheight
                ++ ", w: " ++ show placementpagewidth
                ++ " }"
                

buildSigJS signatorydetails@SignatoryDetails { signatoryname, signatorycompany, signatorynumber, signatoryemail, signatorynameplacements, signatorycompanyplacements, signatoryemailplacements, signatorynumberplacements, signatoryotherfields } = 
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

buildJS authordetails signatorydetails = 
    "{ signatories: " ++ sigs
                          ++ ", author: " ++ buildSigJS authordetails
                          ++ " }" where 
                              sigs = if (length signatorydetails) > 0
                                     then (jsArray (map buildSigJS signatorydetails))
                                     else (jsArray [(buildSigJS emptyDetails)])
                                     
                                     
flashRemindMailSent doc signlink =  
                case signlink of 
                  SignatoryLink{maybesigninfo = Nothing} -> (BS.fromString "En påminnelse har skickats till ") `BS.append` personname
                  _ -> (BS.fromString "Dokumentet har skickats till ") `BS.append` personname
                 where
                   personname =  if (BS.null $ signatoryname $ signatorydetails signlink)
                                  then  signatoryname $ signatorydetails signlink
                                  else signatoryemail $ signatorydetails signlink
                               