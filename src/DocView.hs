{-# LANGUAGE IncoherentInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module DocView where
import AppView
import Data.List
import DocState
import HSP
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import qualified HSX.XMLGenerator as HSX (XML)
import qualified HSX.XMLGenerator
import User
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import KontraLink

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
landpageSignInviteView ctx document =
    <div class="centerdivnarrow">
     <p class="headline">Avtal undertecknat!</p>
     
     <p>Du har undertecknat avtalet och en inbjudan har nu skickats till 
     <span id="mrx"><% concatSignatories (map signatorydetails $ documentsignatorylinks document) %></span>.</p>

     <p><a class="bigbutton" href="/">Skapa ett nytt avtal</a></p>
     <p><a class="secbutton" href="/issue">Avsluta</a></p>
    </div>

landpageSignedView :: (XMLGenerator m) => 
                      Context -> 
                      Document -> 
                      SignatoryLinkID -> 
                      XMLGenT m (HSX.XML m)
landpageSignedView ctx document signatorylinkid =
    <div class="centerdivnarrow">
      <p class="headline">Avtalet <strong><% documenttitle document %></strong> är färdigställt!</p>

      <p>Alla parter har undertecknat avtalet och du har fått en låst PDF kopia av avtalet i din inkorg. Vi rekommenderar att du sparar avtalet online via vår tjänst. Då kommer du ha tillgång till avtalet oavsett vad som händer med breven i din inkorg. Det kostar ingenting och är du snabb tar det endast ca 15-60 sekunder att spara avtalet.</p>

     <a class="bigbutton" href=("/landpage/signedsave/" ++ (show $ documentid document) ++ "/" ++ show signatorylinkid)>Spara avtal</a>
     <a class="secbutton" href=("/sign/" ++ (show $ documentid document) ++ "/" ++ show signatorylinkid)>Nej tack, jag är klar</a>
    </div>

landpageLoginForSaveView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
                         => Context -> Document -> SignatoryLinkID -> XMLGenT m (HSX.XML m)
landpageLoginForSaveView ctx document signatorylinkid =
    let loginlink = maybeSignInLink2 ctx "Login" (LinkLandpageSaved document signatorylinkid) 
                    "bigbutton" in
    <div class="centerdivnarrow">
        <a class="headline">Login</a>
        <p>För att du ska kunna komma åt ditt avtal i framtiden skapar vi ett konto till dig.</p>

        <p>Vår filosofi är att göra det så enkelt som möjligt för våra kunder. Därför använder vi openID som inloggningsmetod. Då behöver du inte krångla med att komma ihåg ytterligare ett användarnamn och en till kod. Välj den tjänst du vill använda för att logga in hos oss i framtiden.</p>

        <p><% loginlink %></p>
       </div>

landpageDocumentSavedView :: (XMLGenerator m) => Context -> Document -> SignatoryLinkID -> XMLGenT m (HSX.XML m)
landpageDocumentSavedView (ctx@Context { ctxmaybeuser = Just user }) signatorylinkid document = 
    <div class="centerdivnarrow">
     <p class="headline">Välkommen <% userfullname user %>!</p>

     <p>Ditt dokument är nu sparat. Du finner dokumentet under Avtal.</p>
 
     <p>Vi hoppas att du är nöjd med vår tjänst hittills och att du är nyfiken på att själv använda SkrivaPå för att skriva dina avtal. Därför erbjuder vi dig som ny kund möjligheten att testa tjänsten genom tre fria avtal. Dina fria avtal förbrukas endast då ett avtal undertecknats av alla parter.</p>

     <p>Börja redan nu! Ladda upp ditt avtal genom att klicka nedan.</p>
     <a class="bigbutton" href="/">Starta</a> {- FIXME: move upload stuff here also -}
    </div>

welcomeEmail :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
welcomeEmail fullname =
    <div>
      <p>Hej <% fullname %>,</p>
      <p>Tack för att du har skapat ett konto hos oss! Vi hoppas att du kommer att bli nöjd med våra tjänster.</p>
      <p>Jag heter Lukas Duczko och är VD på SkrivaPå och det här mailet är skickat direkt från min mailadress. Tveka inte att höra av dig med åsikter, feedback eller bara en enkel hälsning.</p>
      <p>MVH<br/>
         /Lukas Duczko och team SkrivaPå
      </p>
    </div>
xxx (Just (XMLMetaData (showDt, dt) _ pr), xml) = 
        BS.fromString ((if showDt then (dt ++) else id) (pr xml))
xxx (Nothing, xml) =
        BS.fromString (renderAsHTML xml)



documentIssuedFlashMessage :: (MonadIO m) => Document -> m FlashMessage
documentIssuedFlashMessage document = liftM (FlashMessage . xxx) $ webHSP1
    <div>
     Du har undertecknat avtalet och en inbjudan har nu skickats till 
     <span id="mrx"><% concatSignatories (map signatorydetails $ documentsignatorylinks document) %></span>.
    </div>

documentSavedForLaterFlashMessage :: (MonadIO m) => Document -> m FlashMessage
documentSavedForLaterFlashMessage document = liftM (FlashMessage . xxx) $ webHSP1
    <div>
     Du har sparat documentet.
    </div>

documentSignedFlashMessage :: (MonadIO m) => Document -> m FlashMessage
documentSignedFlashMessage document = liftM (FlashMessage . xxx) $ webHSP1
    <div>
     Du har undertecknat avtalet!
    </div>

documentClosedFlashMessage :: (MonadIO m) => Document -> m FlashMessage
documentClosedFlashMessage document = liftM (FlashMessage . xxx) $ webHSP1
    <div>
     Du har undertecknat avtalet! Avtalet är undertecknat av alla partner nu!
    </div>
  
mkSignDocLink :: String -> DocumentID -> SignatoryLinkID -> String
mkSignDocLink hostpart documentid signaturelinkid =
    hostpart ++ "/sign/" ++ show documentid ++ "/" ++ show signaturelinkid


instance (XMLGenerator m) => (EmbedAsChild m (Document, Bool)) where
    asChild (entry, alt) = 
          <%
           <tr class=(if alt then "alt" else "")>
            <td>
             <a href=("/issue/" ++ show (documentid entry))><% documenttitle entry %></a>
            </td>
            <td>
             <% show $ documentmtime entry %>
            </td>
            <td>
             <% show (documentstatus entry) %>
            </td>
           </tr>
          %>

instance (XMLGenerator m) => (EmbedAsChild m [Document]) where
    asChild (entries) = 
        <% 
         <table class="commentlist" width="100%">
           <% zip entries (cycle [False,True]) %>
         </table>
        %>

concatSignatories :: [SignatoryDetails] -> String
concatSignatories siglinks = 
    concat $ intersperse ", " $ map (BS.toString . signatoryname) siglinks 

oneDocumentRow document = 
    let link = "/issue/" ++ show (documentid document)
        mk x = <a href=link><% x %></a>
        statusimg = "/theme/images/" ++
                    case documentstatus document of
                      Preparation -> "status_draft.png"
                      Pending  -> "status_pending.png"
                      Closed -> "status_signed.png"
                      Canceled -> "status_rejected.png"
                      Timedout -> "status_timeout.png"
                      Rejected -> "status_rejected.png"
    in
    <tr>
     <td class="tdleft">
      <input type="checkbox"/>
     </td>
     <td><img width="17" height="17" src=statusimg/></td>
     <td><% mk $ concatSignatories (map signatorydetails $ documentsignatorylinks document) %></td>
     <td><% mk $ documenttitle document %></td>
     <td><% mk $ case documenttimeouttime document of
                   Nothing -> "-"
                   -- FIXME: show days to sign, not the final date
                   Just (TimeoutTime x) -> show x
          %>
     </td>
     <td class="tdright"><% mk $ show $ documentmtime document %></td>
    </tr>


listDocuments :: (XMLGenerator m) => [Document] -> XMLGenT m (HSX.XML m)
listDocuments documents = 
    <div>
     <br/>
     <table class="doctable" cellspacing="0">
      <col/>
      <col/>
      <col/>
      <col/>
      <col/>
      <thead>
       <tr>
        <td>Alla</td>
        <td></td> {- status icon -}
        <td>Personer</td>
        <td>Avtal</td>
        <td>Dagar kvar</td>
        <td>Senaste handelse</td>
       </tr>
      </thead>
      <tfoot>
       <tr>
        <td colspan="6" style="text-align: right; overflow: hidden;">
          <img src="/theme/images/status_draft.png"/> Utkast
          <img src="/theme/images/status_rejected.png"/> Avbrutet
          <img src="/theme/images/status_timeout.png"/> Time Out
          <img src="/theme/images/status_pending.png"/> Väntar
          <img src="/theme/images/status_viewed.png"/> Granskat
          <img src="/theme/images/status_signed.png"/> Undertecknat
         </td>
       </tr>
      </tfoot>
      <tbody>
       <% map oneDocumentRow (filter (not . documentdeleted) documents) %>
      </tbody>
     </table>
   </div>

showFile
  :: (EmbedAsChild m String) =>
     File -> XMLGenT m (HSX.XML m)
showFile file = <li><% show file %></li>

showSignatory
  :: (EmbedAsChild m String, Show a) => a -> XMLGenT m (HSX.XML m)
showSignatory sig = <li><% show sig %></li>



showSignatoryEntryForEdit :: (XMLGenerator m) => DocState.SignatoryDetails -> XMLGenT m (HSX.XML m)
showSignatoryEntryForEdit (SignatoryDetails{signatoryname,signatorycompany,signatorynumber, signatoryemail}) = 
    showSignatoryEntryForEdit2 "" (BS.toString signatoryname) 
                                   (BS.toString signatorycompany) 
                                   (BS.toString signatorynumber) 
                                   (BS.toString signatoryemail)

showSignatoryEntryForEdit2 :: (XMLGenerator m) => String -> String -> String -> String
                           -> String -> XMLGenT m (HSX.XML m)
showSignatoryEntryForEdit2 idx signatoryname signatorycompany signatorynumber signatoryemail = 
    <li id=idx>
      <label>Namn på avtalspart</label><br/> 
      <input name="signatoryname" type="text" value=signatoryname/><br/>
      <label>Titel, företag</label><br/>
      <input name="signatorycompany" type="text" value=signatorycompany/><br/>
      <label>Orgnr/Persnr</label><br/>
      <input name="signatorynumber" type="text" value=signatorynumber/><br/>
      <label>Personens e-mail</label><br/>
      <input name="signatoryemail" type="text" value=signatoryemail/><br/>
      <a onclick="return signatoryremove(this);" href="#">Ta bort</a>
      {- days to sign:
         Antal dagar att skriva på -}
    </li>

showSignatoryEntryStatus :: (XMLGenerator m) => SignatoryLink -> XMLGenT m (HSX.XML m)
showSignatoryEntryStatus (SignatoryLink{signatorydetails = SignatoryDetails{signatoryname,signatoryemail}
                                       ,maybeseentime,maybesigninfo}) = 
    <li> 
        <b><% signatoryname %></b><br/>
        <% case maybesigninfo of
             Just (SignInfo{signtime}) -> "Undertecknat " ++ show signtime 
             Nothing -> case maybeseentime of
                          Just time -> "Har öppnat dokumentet " ++ show time
                          Nothing -> "Har inte öppnat dokumentet"
        %>
    </li>

 -- FIXME: add info about date viewed, date signed, send reminder, change email
showFileImages file = 
   [ <img src=("/pages/" ++ show (fileid file) ++ "/" ++ show pageno) width="300"/> |
     pageno <- [1..(length (filejpgpages file))]]

showFilesImages2 files = <xml><% concatMap showFileImages files %></xml> 

showDocumentBox document = 
    <div id="documentBox">
        {- <% map showFileImages (files document) %> -}
        Förbereder avtal...
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
          }

showDocument :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
             => User 
             -> Document 
             -> Bool 
             -> Int                -- free documents left
             -> XMLGenT m (HSX.XMLGenerator.XML m)
showDocument user document issuedone freeleft =
   let helper = [ <span style="display: none">
                   <% showSignatoryEntryForEdit2 "signatory_template" "" "" "" "" %>
                  <div id="dialog-confirm-signinvite" title="Underteckna">
	        
            <p> När du bekräftar avtalet kommer en automatisk inbjudan att skickas till 
                <strong><span id="mrx">"Mr X"</span></strong>. 
             <strong>Avtalet blir juridiskt bindande när alla parter undertecknat.</strong>
            </p>
            
            {-
            <p>Det är först då vi tar betalt. 
            Vi fakturerar månadsvis. Era fakturauppgifter:</p>

            <div class="inlinebox">
            Referens: <% userfullname user %> <br/>
            Företag: <% usercompanyname user %> <br/>
            Org nr: <% usercompanynumber user %> <br/>
            Adress: <% userinvoiceaddress user %> <br/>
            Pris: ”20 SEK exkl moms” <br/>
            </div>
            -}

           <p>Är du säker på att du vill underteckna avtalet?</p>

          </div>

                <div id="dialog-confirm-signinvite-done" title="Avtal undertecknat!">
	        <p> Du har undertecknat avtalet och en inbjudan har nu skickats till 
                        <span id="mrx"><% concatSignatories (map signatorydetails $ documentsignatorylinks document) %></span>.</p>

          </div>
        </span>
                , <script> var documentid = <% show $ documentid document %>; 
                           var issuedone = <% if issuedone then "true" else "false" %>;
                  </script>
                , <script type="text/javascript" src="/js/document-edit.js"/>
                ]
   in showDocumentPageHelper (LinkIssueDoc document) document helper 
           (BS.fromString $ "Avtal: " ++ BS.toString (documenttitle document))  
      <div>
       <div><strong>1. Personer</strong>

        <% if documentstatus document == Preparation
           then 
             <div>
              <ol id="signatorylist">
               <% map showSignatoryEntryForEdit (if null (documentsignatorylinks document)
                                                 then [emptyDetails] 
                                                 else map signatorydetails $ documentsignatorylinks document) %>
              </ol>
              <a onclick="signatoryadd(); return false;" href="#">Lägg till fler</a>
             </div>
           else
             <div>
              <ol id="signatorylist">
               <% map showSignatoryEntryStatus (documentsignatorylinks document) %>
              </ol>
             </div>              
         %>
         <hr/>

         <strong>2. Pris</strong><br/>
         Pris: 20kr exkl moms<br/>
         Betalningssätt: 
         <% if documentchargemode document == ChargeInitialFree
                then <% show (freeleft+1) ++ " fria avtal kvar" %>
                else <% "Faktura" %>
         {- Namnteckning: [Enkel, 10 kr/st] -}
         {- Antal: ”2st” -}
         %><br/>
         OBS! Du betalar endast när alla parter undertecknat.
         <hr/>
         <strong>3. Avtal</strong><br/>
         <% 
           if (documentstatus document==Preparation) 
              then <span>
                    Dagar kvar: <input type="text" name="daystosign" value=(documentdaystosign document) maxlength="2" size="2"/><br/>
                    <input class="bigbutton" type="submit" name="final" value="Underteckna" id="signinvite"/>
                    <input type="hidden" name="final2" value=""/>
                    <br/>
                    <input class="secbutton" type="submit" name="save" value="Spara till senare"/>
                   </span>
              else <span/>
          %>
       </div>
      </div>

showDocumentPageHelper
    :: (XMLGenerator m, HSX.XMLGenerator.EmbedAsChild m c,
                     EmbedAsAttr m (Attr [Char] KontraLink),
                     HSX.XMLGenerator.EmbedAsChild m d) =>
     KontraLink
     -> DocState.Document
     -> c
     -> BS.ByteString
     -> d
     -> XMLGenT m (HSX.XML m)
showDocumentPageHelper action document helpers title content =
   <div> 
   <br/>
   <% helpers %>
   <form method="post" id="form" name="form" action=action> 
    <table class="docview">
     <tr>
      <td>
   
       <% showDocumentBox document %>
      </td>
      <td> 
       <p class="headline"><% title %><br/> 
           <small><a href=("/issue/" ++ show (documentid document) ++ "/" ++ BS.toString title ++ ".pdf") target="_blank">Öppna som PDF</a></small>
       </p>
       <% content %>
      </td>
     </tr>
    </table> 
   </form>
   <div id="dialog-confirm-sign" title="Underteckna">


        <p><strong>Avtalet blir juridiskt bindande när alla parter undertecknat</strong>. 
           Då får du ett e-mail med det färdiga avtalet.</p>
        
        <p>Är du säker på att du vill underteckna avtalet?</p>

   </div>

   </div>


showDocumentForSign :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) =>
                       KontraLink -> Document -> BS.ByteString -> BS.ByteString -> Bool 
                    -> XMLGenT m (HSX.XML m)
showDocumentForSign action document authorname invitedname wassigned =
   let helper = [ <script type="text/javascript" src="/js/document-edit.js"/>
                , <script> var documentid = <% show $ documentid document %>; 
                  </script>
                ]
   in showDocumentPageHelper action document helper 
              (BS.fromString $ "Avtal: " ++ BS.toString(documenttitle document)) $
        if wassigned 
           then <span>Du har redan skrivit på!</span>
           else <span>
                
                <p>Välkommen <% invitedname %>,</p>

                <p>Genom SkrivaPå kan du underteckna juridiskt bindande avtal online. 
                   På vänster sida har du avtalet <strong><% documenttitle document %></strong> 
                   som <strong><% authorname %></strong> har bjudit in dig att underteckna.
                </p>

                 {-
                <p>Om du inte är <strong><% invitedname %></strong> ber vi dig att avvisa avtalet.</p>
                 -}

                 {- Avvisa - gray FIXME -}

                   <input type="hidden" name="sign2" value=""/>
                   <input class="bigbutton" type="submit" name="sign" value="Underteckna" id="sign"/>
                 <p>Jag vill veta mer <a href="/about" target="_blank">om SkrivaPå</a>.
                 </p>
                </span>



invitationMailXml :: (XMLGenerator m) 
                  => Context
                  -> BS.ByteString
                  -> BS.ByteString
                  -> BS.ByteString
                  -> DocumentID
                  -> SignatoryLinkID
                  -> XMLGenT m (HSX.XML m)
invitationMailXml (Context {ctxmaybeuser = Just user, ctxhostpart}) 
                  emailaddress personname 
                  documenttitle documentid 
                  signaturelinkid = 
    let link = ctxhostpart ++ "/sign/" ++ show documentid ++ "/" ++ show signaturelinkid
        creatorname = BS.toString $ userfullname user
    in 
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
     </head>
     <body>
      <p>Hej <% personname %>,</p>
      <p></p>
      <p><strong><% creatorname %></strong> har bjudit in dig att skriva på avtalet 
         <strong><% documenttitle %></strong>. Klicka på länken nedan för att öppna avtalet. 
         Du undertecknar genom att bekräfta avtalet i nästa steg.</p>
      <p><a href=link><% link %></a></p>
      <% poweredBySkrivaPaPara %>
     </body>
    </html>

invitationMail :: Context
               -> BS.ByteString
               -> BS.ByteString
               -> BS.ByteString
               -> DocumentID
               -> SignatoryLinkID
               -> IO BS.ByteString
invitationMail ctx emailaddress personname 
               documenttitle documentid signaturelinkid = do
                 let xml = invitationMailXml ctx emailaddress personname 
                           documenttitle documentid signaturelinkid
                           -- FIXME: first part of tuple is Maybe Metadata
                           -- potentially important
                 (_,content) <- evalHSP Nothing xml
                 return (BS.fromString (renderAsHTML content))

closedMailXml :: (XMLGenerator m) 
                  => Context
                  -> BS.ByteString
                  -> BS.ByteString
                  -> BS.ByteString
                  -> DocumentID
                  -> SignatoryLinkID
                  -> XMLGenT m (HSX.XML m)
closedMailXml (Context {ctxhostpart}) 
              emailaddress personname 
              documenttitle documentid 
              signaturelinkid = 
    let link = ctxhostpart ++ "/sign/" ++ show documentid ++ "/" ++ show signaturelinkid
    in 
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
     </head>
     <body>
      <p>Hej <% personname %>,</p>
      <p>Avtalet <strong><% documenttitle %></strong> har undertecknats av alla parter. 
         Avtalet är nu lagligt bindande.</p>
      
      <p>Det färdiga avtalet bifogas nedan. Om du har ett konto hos SkrivaPå hittar du avtalet 
         under "Avtal". Om du inte har ett konto kan du spara avtalet genom att klicka på länken:</p>

      <p><a href=link><% link %></a></p>
     
      <% poweredBySkrivaPaPara %>
     </body>
    </html>

closedMail :: Context
           -> BS.ByteString
           -> BS.ByteString
           -> BS.ByteString
           -> DocumentID
           -> SignatoryLinkID
           -> IO BS.ByteString
closedMail ctx emailaddress personname 
               documenttitle documentid signaturelinkid = do
                 let xml = closedMailXml ctx emailaddress personname 
                           documenttitle documentid signaturelinkid
                           -- FIXME: first part of tuple is Maybe Metadata
                           -- potentially important
                 (_,content) <- evalHSP Nothing xml
                 return (BS.fromString (renderAsHTML content))

closedMailAuthorXml :: (XMLGenerator m) 
                     => Context
                  -> BS.ByteString
                  -> BS.ByteString
                  -> BS.ByteString
                  -> DocumentID
                  -> XMLGenT m (HSX.XML m)
closedMailAuthorXml (Context {ctxhostpart}) 
                  emailaddress personname 
                  documenttitle documentid = 
    let link = ctxhostpart ++ "/issue/" ++ show documentid
    in 
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
     </head>
     <body>
      <p>Hej <% personname %>,</p>
      <p>Avtalet <strong><% documenttitle %></strong> har undertecknats av alla parter. 
         Avtalet är nu lagligt bindande.</p>
      
      <p>Det färdiga avtalet bifogas nedan. Hos SkrivaPå hittar du avtalet under "Avtal". </p>

      <p><a href=link><% link %></a></p>
     
      <% poweredBySkrivaPaPara %>
     </body>
    </html>

closedMailAuthor :: Context
           -> BS.ByteString
           -> BS.ByteString
           -> BS.ByteString
           -> DocumentID
           -> IO BS.ByteString
closedMailAuthor ctx emailaddress personname 
               documenttitle documentid = do
                 let xml = closedMailAuthorXml ctx emailaddress personname 
                           documenttitle documentid
                           -- FIXME: first part of tuple is Maybe Metadata
                           -- potentially important
                 (_,content) <- evalHSP Nothing xml
                 return (BS.fromString (renderAsHTML content))

poweredBySkrivaPaPara :: (XMLGenerator m) => XMLGenT m (HSX.XML m)
poweredBySkrivaPaPara = 
    <p>
      {- Med vänliga hälsningar<br/> -}
     <small>Powered by <a href="http://skrivapa.se/">SkrivaPå</a></small>
    </p>

passwordChangeMailXml :: (XMLGenerator m) 
                     => BS.ByteString
                  -> BS.ByteString
                  -> BS.ByteString
                  -> XMLGenT m (HSX.XML m)
passwordChangeMailXml emailaddress personname newpassword = 
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
     </head>
     <body>
      <p>Hej <% personname %>,</p>

      <p>Jag heter Lukas Duczko och är VD på SkrivaPå. Tack för att du har skapat ett konto hos oss. 
         Vi hoppas att du kommer att bli nöjd med våra tjänster. Tveka inte att höra av dig med 
         åsikter, feedback eller bara en enkel hälsning. Din åsikt är värdefull.</p>

      <p>Dina användaruppgifter på SkrivaPå</p>
      <p>Användarnamn: <% emailaddress %><br/>
         Lösenord: <% newpassword %><br/>
      </p>
      <p>
      http://skrivapa.se/login
      </p>

      <p>MVH<br/>
         /Lukas Duczko och team SkrivaPå
      </p>
     </body>
    </html>

passwordChangeMail :: BS.ByteString
                   -> BS.ByteString
                   -> BS.ByteString
                   -> IO BS.ByteString
passwordChangeMail emailaddress personname newpassword = do
                 let xml = passwordChangeMailXml emailaddress personname newpassword
                           -- FIXME: first part of tuple is Maybe Metadata
                           -- potentially important
                 (_,content) <- evalHSP Nothing xml
                 return (BS.fromString (renderAsHTML content))
