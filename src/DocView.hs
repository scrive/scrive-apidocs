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
landpageSignInviteView ctx document =
    <div class="centerdivnarrow">
     <p class="headline">Avtal undertecknat!</p>
     
     <p>Du har undertecknat avtalet och en inbjudan har nu skickats till 
      <strong><span id="mrx"><% concatSignatories (map signatorydetails $ documentsignatorylinks document) %></span></strong>.
     </p>

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
       {- change "alla parter" to list of people -}
      <p>Alla parter har undertecknat avtalet och du har fått en PDF kopia av avtalet i din inkorg. Vi rekommenderar att du sparar avtalet online via vår tjänst. Då kommer du ha tillgång till avtalet oavsett vad som händer med breven i din inkorg. Det kostar ingenting och är du snabb tar det endast ca 60 sekunder att spara avtalet.</p>

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

        <p><% loginlink %></p>
       </div>

landpageDocumentSavedView :: (XMLGenerator m) => Context -> Document -> SignatoryLinkID -> XMLGenT m (HSX.XML m)
landpageDocumentSavedView (ctx@Context { ctxmaybeuser = Just user }) signatorylinkid document = 
    <div class="centerdivnarrow">
     <p class="headline">Välkommen <strong><% userfullname user %></strong>!</p>

     <p>Ditt dokument är nu sparat. Du finner dokumentet under Avtal.</p>
 
     <p>Vi hoppas att du är nöjd med vår tjänst hittills och att du är nyfiken på att själv använda SkrivaPå för att skriva dina avtal. Därför erbjuder vi dig som ny kund möjligheten att testa tjänsten genom tre fria avtal. Dina fria avtal förbrukas endast då ett avtal undertecknats av alla parter.</p>
      {- change "alla parter" to list of people -}

     <p>Börja redan nu! Ladda upp ditt avtal genom att klicka nedan.</p>
     <a class="bigbutton" href="/">Starta</a> {- FIXME: move upload stuff here also -}
    </div>

welcomeEmail :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
welcomeEmail fullname =
    <div>
      <p>Hej <strong><% fullname %></strong>,</p>
      
      <p>Jag heter <strong>Lukas Duczko</strong> och är VD på <strong>SkrivaPå</strong>. Tack för att du har skapat ett konto hos oss. Vi hoppas att du
kommer att bli nöjd med våra tjänster. Tveka inte att höra av dig med åsikter,
feedback eller bara en enkel hälsning. Din åsikt är värdefull.</p>
      <p>MVH<br/>
         <strong>Lukas Duczko</strong> och team <a href="http://skrivapa.se/">SkrivaPå</a>
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
     <strong><span id="mrx"><% concatSignatories (map signatorydetails $ documentsignatorylinks document) %></span></strong>.
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
    <div>  {- change "alla parter" to list of people -}
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

oneDocumentRow document@Document{ documentid, documentsignatorylinks
                                , documentstatus, documenttitle
                                , documenttimeouttime
                                , documentmtime }  = 
    let link = LinkIssueDoc document
        mk x = <a href=link><% x %></a>
        statusimg = "/theme/images/" ++
                    case documentstatus of
                      Preparation -> "status_draft.png"
                      Pending  -> "status_pending.png"
                      Closed -> "status_signed.png"
                      Canceled -> "status_rejected.png"
                      Timedout -> "status_timeout.png"
                      Rejected -> "status_rejected.png"
    in
    <tr>
     <td class="tdleft">
      <input type="checkbox" name="doccheck" value=documentid/>
     </td>
     <td><img width="17" height="17" src=statusimg/></td>
     <td><% mk $ concatSignatories (map signatorydetails documentsignatorylinks) %></td>
     <td><% mk $ documenttitle %></td>
     <td><% mk $ case documenttimeouttime of
                   Nothing -> "-"
                   -- FIXME: show days to sign, not the final date
                   Just (TimeoutTime x) -> show x
          %>
     </td>
     <td><% mk $ show $ documentmtime %></td>
     <td class="tdright"></td>
    </tr>


listDocuments :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink),
                      EmbedAsAttr m (Attr [Char] DocumentID)) => [Document] -> XMLGenT m (HSX.XML m)
listDocuments documents = 
    <div>
     <br/>
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
        <td>Personer</td>
        <td>Avtal</td>
        <td>Förfallodatum</td>
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
           <img src="/theme/images/status_pending.png"/> Väntar
           <img src="/theme/images/status_viewed.png"/> Granskat
           <img src="/theme/images/status_signed.png"/> Undertecknat
          </div>
          <div class="clearboth"/>
         </td>
       </tr>
      </tfoot>
      <tbody>
       <% map oneDocumentRow (filter (not . documentdeleted) documents) %>
      </tbody>
     </table>
     </form>
   </div>

showFile
  :: (EmbedAsChild m String) =>
     File -> XMLGenT m (HSX.XML m)
showFile file = <li><% show file %></li>

showSignatory
  :: (EmbedAsChild m String, Show a) => a -> XMLGenT m (HSX.XML m)
showSignatory sig = <li><% show sig %></li>



showSignatoryEntryForEdit :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink),
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
    <li id=idx>
      {- <label>Namn på avtalspart</label><br/> -}
      <input name="signatoryname" type="text" value=signatoryname
             infotext="Namn på avtalspart"/><br/>
      {- <label>Titel, företag</label><br/> -}
      <input name="signatorycompany" type="text" value=signatorycompany
             infotext="Titel, företag"/><br/>
      {- <label>Orgnr/Persnr</label><br/> -}
      <input name="signatorynumber" type="text" value=signatorynumber
             infotext="Orgnr/Persnr"/><br/>
      {- <label>Personens e-mail</label><br/> -}
      <input name="signatoryemail" type="text" value=signatoryemail
             infotext="Personens e-mail"/><br/>
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
   [ <img class="pagejpg" src=("/pages/" ++ show (fileid file) ++ "/" ++ show pageno) width="300"/> |
     pageno <- [1..(length (filejpgpages file))]]

showFilesImages2 files = <span><% concatMap showFileImages files %></span> 

showDocumentBox document = 
    <div id="documentBox">
        {- <% map showFileImages (files document) %> -}
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
showDocument user document issuedone freeleft =
   let helper = [ <span style="display: none">
                   <% showSignatoryEntryForEdit2 "signatory_template" "" "" "" "" %>
                  <div id="dialog-confirm-signinvite" title="Underteckna">
	        
            <p> När du bekräftar avtalet kommer en automatisk inbjudan att skickas till 
                <strong><span id="mrx">"Mr X"</span></strong>.
             <strong>Avtalet blir juridiskt bindande när alla parter undertecknat.</strong>
            </p>
              {- change "alla parter" to list of people -} 
            
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
                        <strong><span id="mrx"><% concatSignatories (map signatorydetails $ documentsignatorylinks document) %></span></strong>.</p>

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
             <div id="persons">
              {- <label>Ditt namn</label><br/> -}
              <input name="authorname" type="text" value=(signatoryname $ documentauthordetails document)
                     infotext="Ditt namn"/><br/>
              {- <label>Titel, företag</label><br/> -}
              <input name="authorcompany" type="text" value=(signatorycompany $ documentauthordetails document)
                     infotext="Titel, företag"/><br/>
              {- <label>Ditt Orgnr/Persnr</label><br/> -}
              <input name="authornumber" type="text" value=(signatorynumber $ documentauthordetails document)
                     infotext="Ditt Orgnr/Persnr"/><br/>
              {- <label>Din e-mail</label><br/> -}
              <input name="authoremail" type="text" value=(signatoryemail $ documentauthordetails document)
                     infotext="Din e-mail"/><br/>

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
         20kr exkl moms<br/>
         <hr/>
         <strong>3. Avtal</strong><br/>
         <% 
           if (documentstatus document==Preparation) 
              then <span>
                    Dagar kvar: <input type="text" name="daystosign" value=(documentdaystosign document) maxlength="2" size="2"/><br/>
                    <input class="bigbutton" type="submit" name="final" value="Underteckna" id="signinvite"/>
                    <br/>
                    <input class="secbutton" type="submit" name="save" value="Spara till senare"/>
                   </span>
              else <span/>
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
      <p class="headline"><% title %><br/> 
          <small><a href=(LinkIssueDocPDF document) target="_blank">Öppna som PDF</a></small>
      </p>
      <form method="post" id="form" name="form" action=action> 
       <% content %>
      </form>
     </div>
     <div class="clearboth"/>
    </div> 


showDocumentForSign :: (XMLGenerator m,
                        EmbedAsAttr m (Attr [Char] KontraLink),
                        EmbedAsAttr m (Attr [Char] BS.ByteString)) =>
                       KontraLink -> Document -> BS.ByteString -> BS.ByteString -> Bool 
                    -> XMLGenT m (HSX.XML m)
showDocumentForSign action document authorname invitedname wassigned =
   let helper = [ <script type="text/javascript" src="/js/document-edit.js"/>
                , <script> var documentid = <% show $ documentid document %>; 
                  </script>
                , <div id="dialog-confirm-sign" title="Underteckna">  {- change "alla parter" to list of people -}
                   <p><strong>Avtalet blir juridiskt bindande när alla parter undertecknat</strong>. 
                      Då får du ett e-mail med det färdiga avtalet.</p>
                   <p>Är du säker på att du vill underteckna avtalet?</p>
                  </div>
                ]

   in showDocumentPageHelper action document helper 
              (BS.fromString $ "Avtal: " ++ BS.toString(documenttitle document)) $
        if wassigned 
           then <span>Du har redan undertecknat!</span>
           else <span>
                
                <p>Välkommen <strong><% invitedname %></strong>,</p>

                <p>Genom SkrivaPå kan du underteckna juridiskt bindande avtal online. 
                   På vänster sida har du avtalet <strong><% documenttitle document %> </strong> 
                   som <strong><% authorname %></strong> har bjudit in dig att underteckna.
                </p>

                 {-
                <p>Om du inte är <strong><% invitedname %></strong> ber vi dig att avvisa avtalet.</p>
                 -}

                 {- Avvisa - gray FIXME -}

                   <input class="bigbutton" type="submit" name="sign" value="Underteckna" id="sign"/>
                 <p>Jag vill veta mer <a href="/about" target="_blank">om SkrivaPå</a>.
                 </p>
                </span>


renderHSPToByteString xml = do
  (meta,content) <- evalHSP Nothing xml
  return $ case meta of
             Just (XMLMetaData (showDt, dt) _ pr) -> 
                     BS.fromString ((if showDt then (dt ++) else id) (pr content))
             Nothing -> BS.fromString (renderAsHTML content)



invitationMailXml :: (XMLGenerator m) 
                  => Context
                  -> BS.ByteString
                  -> BS.ByteString
                  -> Document
                  -> SignatoryLinkID
                  -> XMLGenT m (HSX.XML m)
invitationMailXml (Context {ctxmaybeuser = Just user, ctxhostpart}) 
                  emailaddress personname 
                  document@Document{documenttitle,documentid} 
                  signaturelinkid = 
    let link = ctxhostpart ++ "/sign/" ++ show documentid ++ "/" ++ show signaturelinkid
        creatorname = signatoryname (documentauthordetails document)
    in 
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
     </head>
     <body>
      <p>Hej <strong><% personname %></strong>,</p>
      <p></p>
      <p><strong><% creatorname %></strong> har bjudit in dig att underteckna på dokumentet 
         <strong><% documenttitle %></strong>. Klicka på länken nedan för att granska dokumentet online. Du undertecknar genom att bekräfta avtalet i nästa steg.</p>
      <p><a href=link><% link %></a></p>
      <% poweredBySkrivaPaPara %>
     </body>
    </html>

invitationMail :: Context
               -> BS.ByteString
               -> BS.ByteString
               -> Document
               -> SignatoryLinkID
               -> IO BS.ByteString
invitationMail ctx emailaddress personname 
               document signaturelinkid = do
                 let xml = invitationMailXml ctx emailaddress personname 
                           document signaturelinkid
                 renderHSPToByteString xml

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
     <body> {- change "alla parter" to list of people -}
      <p>Hej <strong><% personname %></strong>,</p>
      <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats av alla parter och avtalet är nu lagligt bindande. Nedan bifogas en direktlänk till det färdigställda dokumentet och en PDF-kopia.</p>

      <p>Om du har ett SkrivaPå-konto med denna mailadress så har avtalet sparats automatiskt på detta konto. Om du inte har ett konto eller om detta är en annan mailadress än den som är registrerad hos oss, kan du spara avtalet genom att klicka på länken: </p>

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
                 renderHSPToByteString xml

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
     <body>  {- change "alla parter" to list of people -}
      <p>Hej <strong><% personname %></strong>,</p>
      <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats av alla parter och avtalet är nu lagligt bindande. Nedan bifogas en direktlänk till det färdigställda dokumentet och en PDF-kopia.</p>
      
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
closedMailAuthor ctx emailaddress personname documenttitle documentid = do
                 let xml = closedMailAuthorXml ctx emailaddress personname 
                           documenttitle documentid
                 renderHSPToByteString xml

poweredBySkrivaPaPara :: (XMLGenerator m) => XMLGenT m (HSX.XML m)
poweredBySkrivaPaPara = 
    <p>
     <small>MVH<br/><a href="http://skrivapa.se/">SkrivaPå</a></small>
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
      <p>Hej <strong><% personname %></strong>,</p>

      <p>Jag heter Lukas Duczko och är VD på SkrivaPå. Tack för att du har skapat ett konto hos oss. 
         Vi hoppas att du kommer att bli nöjd med våra tjänster. Tveka inte att höra av dig med 
         åsikter, feedback eller bara en enkel hälsning. Din åsikt är värdefull.</p>

      <p>Dina användaruppgifter på SkrivaPå</p>
      <p>Användarnamn: <span style="color: orange; text-weight: bold"><% emailaddress %></span><br/>
         Lösenord: <span style="color: orange; text-weight: bold"><% newpassword %></span><br/>
      </p>
      <p>
      http://skrivapa.se/login
      </p>

      <p>MVH<br/>
         /Lukas Duczko och team <a href="http://skrivapa.se/">SkrivaPå</a>.
      </p>
     </body>
    </html>

passwordChangeMail :: BS.ByteString
                   -> BS.ByteString
                   -> BS.ByteString
                   -> IO BS.ByteString
passwordChangeMail emailaddress personname newpassword = do
                 let xml = passwordChangeMailXml emailaddress personname newpassword
                 renderHSPToByteString xml
