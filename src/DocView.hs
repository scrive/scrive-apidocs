{-# LANGUAGE IncoherentInstances, TemplateHaskell, NamedFieldPuns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module DocView(webHSP1,emptyDetails,showFilesImages2,showDocument,listDocuments,invitationMail,closedMail,closedMailAuthor,
landpageSignInviteView,landpageSignedView,landpageLoginForSaveView,landpageDocumentSavedView,
showDocumentForSign,documentSavedForLaterFlashMessage,remindMail
) where
import AppView
import Data.List
import DocState
import HSP
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified HSX.XMLGenerator as HSX (XML)
import qualified HSX.XMLGenerator
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

webHSP1' :: (MonadIO m) => 
            Maybe XMLMetaData -> 
            HSP XML -> 
            m (Maybe XMLMetaData, XML)
webHSP1' metadata hsp = liftIO (evalHSP metadata hsp)

webHSP1 :: (MonadIO m) => HSP XML -> m (Maybe XMLMetaData, XML)
webHSP1 hsp = webHSP1' Nothing hsp


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


documentIssuedFlashMessage :: Document -> HSP.HSP HSP.XML
documentIssuedFlashMessage document = 
    <div>
     Du har undertecknat avtalet och en inbjudan har nu skickats 
     till <% partyListButAuthorString document %>.
    </div>

documentSavedForLaterFlashMessage :: Document -> HSP.HSP HSP.XML
documentSavedForLaterFlashMessage document = 
    <div>
     Du har sparat documentet.
    </div>

documentSignedFlashMessage :: Document -> HSP.HSP HSP.XML
documentSignedFlashMessage document =
    <div>
     Du har undertecknat avtalet!
    </div>

documentClosedFlashMessage :: Document -> HSP.HSP HSP.XML
documentClosedFlashMessage document =
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


listDocuments :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink),
                      EmbedAsAttr m (Attr [Char] DocumentID)) 
              => MinutesTime
              -> UserID
              -> [Document] 
              -> XMLGenT m (HSX.XML m)
listDocuments ctime userid documents = 
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

    
    
--This can be dropped!!!!!!!!!!!!!!!!!! (with old resend implementation)    
showSignatoryEntryStatus :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
                            => Document -> SignatoryLink -> XMLGenT m (HSX.XML m)
showSignatoryEntryStatus document (signatorylink@SignatoryLink{ signatorydetails = SignatoryDetails{ signatoryname
                                                                                                   , signatoryemail
                                                                                                   }
                                                              , signatorylinkid
                                                              , maybeseeninfo
                                                              , maybesigninfo
                                                              }) = 
    <div> {- Sänd inbjudan igen, Sänd email-bekräftelse igen -}
        <b><% signatoryname %></b> <a href=(LinkResendEmail document signatorylink)>Sänd inbjudan igen</a><br/>
        <% case maybesigninfo of
             Just (SignInfo{signtime}) -> "Undertecknat " ++ show signtime 
             Nothing -> case maybeseeninfo of
                          Just (SignInfo time ip) -> "Har öppnat dokumentet " ++ show time
                          Nothing -> "Har inte öppnat dokumentet"
        %>
    </div>

showFileImages file@File { fileid, filejpgpages = JpegPages jpgpages } =
   [ <div id=("page" ++ show pageno) class="pagediv"><img class="pagejpg" src=("/pages/" ++ show fileid ++ "/" ++ show pageno) width="300" /></div> |
     pageno <- [1..(length jpgpages)]]
showFileImages file@File { fileid, filejpgpages = JpegPagesPending } = 
   [ <div class="pagejpga4 pagejpg">
      <img class="waiting" src="/theme/images/wait30trans.gif"/>
     </div> ]
showFileImages file@File { fileid, filejpgpages = JpegPagesError normalizelog } = 
   [ <div class="pagejpga4 pagejpg">
      <% normalizelog %>
     </div> ]



showFilesImages2 files = <span><% concatMap showFileImages files %></span> 

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

showDocument :: (XMLGenerator m,
                 EmbedAsAttr m (Attr [Char] KontraLink),
                 EmbedAsAttr m (Attr [Char] DocumentID),
                 EmbedAsAttr m (Attr [Char] BS.ByteString)) 
             => User 
             -> Document 
             -> Bool 
             -> Int                -- free documents left
             -> XMLGenT m (HSX.XMLGenerator.XML m)
showDocument user 
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
                , <div id="dialog-confirm-signinvite" title="Underteckna">
                    <p>Är du säker att du vill underteckna dokumentet <strong><% documenttitle %></strong>?</p>
                    
                    <p>När du undertecknat kommer en automatisk inbjudan att skickas till 
                       <span class="Xinvited">Invited</span> med e-post.</p>
                   </div>
                , <div id="edit-invite-text-dialog" title="Hälsningsmeddelande">
                   <textarea cols="45" rows="6"></textarea>
                  </div>

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
       addbr text | BS.null text = <span/>
       addbr text = <span><% text %><br/></span>

   in showDocumentPageHelper (LinkIssueDoc document) document helper 
           (documenttitle)  
      <div>
       <div>

        <% if documentstatus == Preparation
           then 
             <span>
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
              <a href="#" onclick="editinvitetext(); return false;">Skriv hälsningsmeddelande</a><br/>
              <p>Undertecknas inom (dagar)
              <input type="text" name="daystosign" value=documentdaystosign maxlength="2" size="2" autocomplete="off"/>
              <input type="hidden" name="invitetext" id="invitetext" value= documentinvitetext/>
              </p>
              <div style="height: 2px;"/>
              <input class="bigbutton" type="submit" name="final" value="Underteckna" id="signinvite"/>
              <input class="button" type="submit" name="save" value="Spara som utkast"/>
              </div>
             </span>
           else
              <div id="signatorylist">
                 <% showSignatoryLinkForSign (Just user) document authorlink %>
                 <% map (showSignatoryLinkForSign (Just user) document) allinvited 
                 %>
              </div>
         %>
       </div>
      </div>

showDocumentPageHelper
    :: (XMLGenerator m, 
        HSX.XMLGenerator.EmbedAsChild m c,
        EmbedAsAttr m (Attr [Char] KontraLink),
        HSX.XMLGenerator.EmbedAsChild m d,
        EmbedAsAttr m (Attr [Char] BS.ByteString)) =>
     KontraLink
     -> DocState.Document
     -> c
     -> BS.ByteString
     -> d
     -> XMLGenT m (HSX.XML m)
showDocumentPageHelper action document helpers title content =
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
      <form method="post" id="form" name="form" action=action> 
       <% content %>
      </form>
     </div>
     <div class="clearboth"/>
    </div> 

class ( XMLGenerator m
      , EmbedAsAttr m (Attr [Char] KontraLink)
      , EmbedAsAttr m (Attr [Char] BS.ByteString)) => XMLGenerator2 m


-- cover for a bug that we did forget time of signing
showDateOnly1 (MinutesTime 0) = ""
showDateOnly1 x = showDateOnly x

showSignatoryLinkForSign muser document siglnk@(SignatoryLink{  signatorylinkid 
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
      status = if (wasSigned)
               then <img src="/theme/images/status_signed.png"/> 
               else if (wasSeen)
                    then <img src="/theme/images/status_viewed.png"/> 
                    else <img src="/theme/images/status_pending.png"/> 
      message =  if (wasSigned)
                 then "Undertecknat " ++ showDateOnly1 (signtime $ fromJust maybesigninfo)
                 else if (wasSeen)
                    then "Granskat " ++ showDateOnly (signtime $ fromJust maybeseeninfo)
                    else  "Har ej undertecknat"         
      isCurrentUserAuthor = (isJust muser) && isAuthor (fromJust muser) document    
      isCurrentSignatorAuthor = (fmap (unEmail . useremail) muser) ==  (Just signatoryemail)    
      reminderText = if (wasSigned)
                     then "Skicka dokumentet igen"
                     else "Skicka påminnelse"
      reminderhref = LinkRemind document siglnk       
      reminderLink = <a href=reminderhref >  <% reminderText %>  </a>
   in asChild <p><% 
                [asChild status,asChild " "] ++
                (if BS.null signatoryname then [] else [ asChild <strong><% signatoryname %></strong>, asChild <br/> ]) ++
                (if BS.null signatorycompany then [] else [ asChild signatorycompany, asChild <br/> ]) ++
                (if BS.null signatorynumber then [] else [ asChild signatorynumber, asChild <br/> ]) ++
                (if BS.null signatoryemail then [] else [ asChild signatoryemail, asChild <br/> ]) ++
                ([asChild message]) ++
                (if (isCurrentUserAuthor && (not isCurrentSignatorAuthor)) then [asChild <br/> ,asChild reminderLink] else [])
                %></p>

showDocumentForSign :: ( XMLGenerator m
      , EmbedAsAttr m (Attr [Char] KontraLink)
      , EmbedAsAttr m (Attr [Char] BS.ByteString)) =>
                       KontraLink 
                           -> Document 
                           -> (Maybe User)
                           -> SignatoryLink
                           -> Bool 
                    -> XMLGenT m (HSX.XML m)
showDocumentForSign action document muser invitedlink wassigned =
   let helper = [ <script> var documentid = "<% show $ documentid document %>"; 
                  </script>
                , <div id="dialog-confirm-sign" title="Underteckna">  
                   
                   <p>Är du säker att du vill underteckna dokumentet <strong><% documenttitle document %></strong>?</p>
                   <p>När <% partyUnsignedMeAndListString magichash document %> undertecknat blir 
                      avtalet <strong>juridiskt bindande</strong> och
                      det färdigställda avtalet skickas till din e-post.</p>
                  </div>
                ]
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
                                         
   in showDocumentPageHelper action document helper 
              (documenttitle document) $
              <span>
       
                 <p>Vänligen var noga med att granska dokumentet och kontrollera 
                    uppgifterna nedan innan du undertecknar.</p>   

                 <% showSignatoryLinkForSign muser document invitedlink %>

                 <% map (showSignatoryLinkForSign muser document) (authorlink : allbutinvited) %>
                 
                 <%
                   if wassigned 
                     then <span>Du har redan undertecknat!<br/></span>
                     else
                        <span>
                           {- Avvisa - gray FIXME -}
                           <input class="bigbutton" type="submit" name="sign" value="Underteckna" id="sign"/>
                           <br/>
                        </span>
                 %>
                 <small>Jag vill veta mer <a href="/about" target="_blank">om SkrivaPå</a>.</small>
              </span>



invitationMail :: Context
               -> BS.ByteString
               -> BS.ByteString
               -> Document
               -> SignatoryLink
               -> MagicHash
               -> IO Mail
invitationMail (Context {ctxmaybeuser = Just user, ctxhostpart}) 
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
        paragraphs :: [BS.ByteString]
        paragraphs = BS.split 10 documentinvitetext
        -- p :: [GenChildList IO]
        p = intersperse (asChild <br/>) (map asChild paragraphs)
        --authorversion  :: (XMLGenerator m) => GenChildList m
        authorversion = sequence $
               p ++ 
               [ asChild common
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

closedMail :: Context
           -> BS.ByteString
           -> BS.ByteString
           -> Document
           -> SignatoryLink
           -> MagicHash
           -> IO Mail
closedMail (Context {ctxhostpart}) 
              emailaddress personname 
              document@Document{documenttitle,documentid} 
              signaturelink magichash = 
    do
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

closedMailAuthor :: Context
                 -> BS.ByteString
                 -> BS.ByteString
                 -> Document
                 -> IO Mail
closedMailAuthor (Context {ctxhostpart}) 
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

remindMail:: Context -> Document -> SignatoryLink -> IO Mail
remindMail c d s = case s of 
                       SignatoryLink{maybesigninfo = Nothing} -> remindMailNotSigned c d s
                       _ -> remindMailSigned c d s

  
remindMailNotSigned:: Context -> Document -> SignatoryLink -> IO Mail
remindMailNotSigned ctx@Context{ctxmaybeuser = Just user, ctxhostpart}
           document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails,documentinvitetext} 
           signlink = 
    let link = ctxhostpart ++ show (LinkSignDoc document signlink)
        creatorname = signatoryname documentauthordetails
        personname = if (BS.null $ signatoryname $ signatorydetails signlink)
                      then  signatoryname $ signatorydetails signlink
                      else signatoryemail $ signatorydetails signlink
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
            [ asChild common
            , asChild (poweredBySkrivaPaPara ctxhostpart)
            ]
        paragraphs :: [BS.ByteString]
        paragraphs = BS.split 10 documentinvitetext
        -- p :: [GenChildList IO]
        p = intersperse (asChild <br/>) (map asChild paragraphs)
        --authorversion  :: (XMLGenerator m) => GenChildList m
        authorversion = sequence $
               p ++ 
               [ asChild common
               , asChild "Hälsningar"
               , asChild <br/>
               , asChild creatorname
               , asChild <br/>
               ]
        content =   <p> <%creatorname%> vill påminna dig om att du inte undertecknat dokument <%documenttitle%> ännu
                        <br/>
                        <% if BS.null documentinvitetext
                         then <span><% skrivapaversion %></span>
                         else <span><% authorversion %></span>
                        %> 
                    </p>     
              
        title =  BS.concat [BS.fromString "Hej ",personname]             
    in 
       do
        content <- htmlHeadBodyWrapIO documenttitle content
        return $ emptyMail {title = title, content = content}

remindMailSigned:: Context -> Document -> SignatoryLink -> IO Mail
remindMailSigned ctx@Context{ctxmaybeuser = Just user, ctxhostpart}
                 document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails,documentinvitetext} 
                 signlink = 
                     let 
                         personname = if (BS.null $ signatoryname $ signatorydetails signlink)
                                      then signatoryname $ signatorydetails signlink
                                      else signatoryemail $ signatorydetails signlink
                         title =  BS.concat [BS.fromString "Hej ",personname]              
                         content = <span>
                                     <p>Hej <% userfullname user %>,</p>
                                     <p><% personname %> har på din begäran skickat ut dokumentet <% documenttitle %> som du  
                                     har undertecknat via tjänsten SkrivaPå. Dokumentet bifogas nedan.</p>
                                     <% poweredBySkrivaPaPara ctxhostpart %>
                                   </span>
                         attachmentcontent = filepdf $ head $ documentfiles document          
                     in do
                       content <- htmlHeadBodyWrapIO documenttitle content
                       return $ emptyMail {title = title, content = content, attachments = [(documenttitle,attachmentcontent)]}

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