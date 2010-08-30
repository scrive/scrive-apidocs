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
import Misc

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
     
     <p>En inbjudan till avtalet har nu skickats till 
      <strong><span id="mrx"><% concatSignatories (map signatorydetails documentsignatorylinks) %></span></strong>.
     </p>

     <p><a class="bigbutton" href="/">Skapa ett nytt avtal</a></p>
    </div>


{-

Variables:
1. all signed?
2. has account already?
3. is logged in as the account?

Let as skip 3 for now.
-}

willCreateAccountForYou False = <span/>
willCreateAccountForYou True = 
     <p>
       Since you don't have an account at SkrivaPa we can create one for you. You will be able to see all documents
       you have signed with our service and track progress. Det kostar ingenting och 
       tar inte mer än en minut.
     </p>

landpageSignedView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) => 
                      Context -> 
                      Document -> 
                      SignatoryLink -> 
                      Bool ->
                      XMLGenT m (HSX.XML m)
landpageSignedView ctx document@Document{documenttitle,documentstatus} signatorylink hasaccount 
    | documentstatus == Closed =
    <div class="centerdivnarrow">
      <p class="headline">Dokumentet <strong><% documenttitle %></strong> är färdigställt!</p>
      {- change "alla parter" to list of people -}
      <p>Alla parter har undertecknat avtalet och du har fått en PDF kopia av dokumentet i din inkorg.
         Vi rekommenderar att du sparar dokumentet online via vår tjänst.</p>
      <% willCreateAccountForYou (not hasaccount) %>
      <a class="bigbutton" href=(LinkLandpageSaved document signatorylink)>Spara</a>
    </div>

landpageSignedView ctx document@Document{documenttitle} signatorylink hasaccount =
    <div class="centerdivnarrow">
      <p class="headline">Dokumentet <strong><% documenttitle %></strong> är färdigställt!</p>
      {- change "alla parter" to list of people -}
      <p>We are waiting for everybody to sign, then you will receive en PDF kopia av dokumentet i din inkorg.
         Meanwhile vi rekommenderar att du sparar dokumentet online via vår tjänst to track progress. </p>
      <% willCreateAccountForYou (not hasaccount) %>
      <a class="bigbutton" href=(LinkLandpageSaved document signatorylink)>Spara</a>
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
landpageDocumentSavedView (ctx@Context { ctxmaybeuser = Just user }) signatorylink document = 
    <div class="centerdivnarrow">
     <p class="headline">Välkommen <strong><% userfullname user %></strong>!</p>

     <p>Ditt dokument är nu sparat. Du finner dokumentet under Avtal when you log in.</p>
 
     <p>Vi hoppas att du är nöjd med vår tjänst hittills och att du är nyfiken på att själv använda 
        SkrivaPå för att skriva dina avtal. Därför erbjuder vi dig som ny kund möjligheten att testa 
        tjänsten genom tre fria avtal. Dina fria avtal förbrukas endast då ett avtal undertecknats av 
        alla parter.</p>
      {- change "alla parter" to list of people -}

     <p>Börja redan nu! Ladda upp ditt avtal genom att klicka nedan.</p>
     <a class="bigbutton" href="/">Starta</a> {- FIXME: move upload stuff here also -}
    </div>

welcomeEmail :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
welcomeEmail fullname =
    <div>
      <p>Hej <strong><% fullname %></strong>,</p>
      
      <p>Jag heter <strong>Lukas Duczko</strong> och är VD på <strong>SkrivaPå</strong>. Tack för att 
      du har skapat ett konto hos oss. Vi hoppas att du kommer att bli nöjd med våra tjänster. Tveka 
      inte att höra av dig med åsikter, feedback eller bara en enkel hälsning. Din åsikt är värdefull.</p>
      <p>MVH<br/>
         <strong>Lukas Duczko</strong> och team <a href="http://skrivapa.se/">SkrivaPå</a>
      </p>
    </div>



documentIssuedFlashMessage :: (MonadIO m) => Document -> m FlashMessage
documentIssuedFlashMessage document = liftM (FlashMessage . renderXMLAsBSHTML) $ webHSP1
    <div>
     Du har undertecknat avtalet och en inbjudan har nu skickats till 
     <strong><span id="mrx"><% concatSignatories (map signatorydetails $ documentsignatorylinks document) %></span></strong>.
    </div>

documentSavedForLaterFlashMessage :: (MonadIO m) => Document -> m FlashMessage
documentSavedForLaterFlashMessage document = liftM (FlashMessage . renderXMLAsBSHTML) $ webHSP1
    <div>
     Du har sparat documentet.
    </div>

documentSignedFlashMessage :: (MonadIO m) => Document -> m FlashMessage
documentSignedFlashMessage document = liftM (FlashMessage . renderXMLAsBSHTML) $ webHSP1
    <div>
     Du har undertecknat avtalet!
    </div>

documentClosedFlashMessage :: (MonadIO m) => Document -> m FlashMessage
documentClosedFlashMessage document = liftM (FlashMessage . renderXMLAsBSHTML) $ webHSP1
    <div>  {- change "alla parter" to list of people -}
     Du har undertecknat avtalet! Avtalet är undertecknat av alla partner nu!
    </div>
  
mkSignDocLink :: String -> Document -> SignatoryLink -> String
mkSignDocLink hostpart documentid signaturelink =
    hostpart ++ show (LinkSignDoc documentid signaturelink)


concatSignatories :: [SignatoryDetails] -> String
concatSignatories siglinks = 
    concat $ intersperse ", " $ map (BS.toString . signatoryname) siglinks 

oneDocumentRow userid document@Document{ documentid
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
                   Just (TimeoutTime x) -> 
                       if documentstatus==Pending || documentstatus==Timedout  
                       then show x
                       else "-"
          %>
     </td>
     <td><% mk $ show $ documentmtime %></td>
     <td class="tdright"></td>
    </tr>


listDocuments :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink),
                      EmbedAsAttr m (Attr [Char] DocumentID)) 
              => UserID
              -> [Document] 
              -> XMLGenT m (HSX.XML m)
listDocuments userid documents = 
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
           <img src="/theme/images/status_pending.png"/> Skickat
           <img src="/theme/images/status_viewed.png"/> Granskat av motpart
           <img src="/theme/images/status_signed.png"/> Undertecknat
          </div>
          <div class="clearboth"/>
         </td>
       </tr>
      </tfoot>
      <tbody>
       <% map (oneDocumentRow userid) (filter (not . documentdeleted) documents) %>
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
    <div id=idx>
      <input name="signatoryname" type="text" value=signatoryname
             infotext="Namn på avtalspart"/><br/>
      <input name="signatorycompany" type="text" value=signatorycompany
             infotext="Titel, företag"/><br/>
      <input name="signatorynumber" type="text" value=signatorynumber
             infotext="Orgnr/Persnr"/><br/>
      <input name="signatoryemail" type="text" value=signatoryemail
             infotext="Personens e-mail"/><br/>
      <a onclick="return signatoryremove(this);" href="#">Ta bort</a>
    </div>

showSignatoryEntryStatus :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
                            => Document -> SignatoryLink -> XMLGenT m (HSX.XML m)
showSignatoryEntryStatus document (signatorylink@SignatoryLink{ signatorydetails = SignatoryDetails{ signatoryname
                                                                                                   , signatoryemail
                                                                                                   }
                                                              , signatorylinkid
                                                              , maybeseentime
                                                              , maybesigninfo
                                                              }) = 
    <div> {- Sänd inbjudan igen, Sänd email-bekräftelse igen -}
        <b><% signatoryname %></b> <a href=(LinkResendEmail document signatorylink)>Sänd inbjudan igen</a><br/>
        <% case maybesigninfo of
             Just (SignInfo{signtime}) -> "Undertecknat " ++ show signtime 
             Nothing -> case maybeseentime of
                          Just time -> "Har öppnat dokumentet " ++ show time
                          Nothing -> "Har inte öppnat dokumentet"
        %>
    </div>

showFileImages file@File { fileid, filejpgpages = JpegPages jpgpages } = 
   [ <img class="pagejpg" src=("/pages/" ++ show fileid ++ "/" ++ show pageno) width="300"/> |
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
                              } 
             issuedone 
             freeleft =
   let helper = [ showSignatoryEntryForEdit2 "signatory_template" "" "" "" ""
                , <div id="dialog-confirm-signinvite" title="Underteckna">
                    <p>Är du säker på att du vill underteckna dokumentet 
                       <strong><% documenttitle %></strong>?</p>
                    <p>När du bekräftat kommer en automatisk inbjudan att skickas till 
                       <strong><span id="mrx">"Mr X"</span></strong> med e-post.
                       Avtalet blir <strong>juridiskt bindande</strong> när alla parter undertecknat.</p>
                       {- change "alla parter" to list of people -} 
                   </div>

                , <div id="dialog-confirm-signinvite-done" title="Dokumentet undertecknat!">
	            <p>Du har undertecknat avtalet och en inbjudan har nu skickats till 
                    <strong><span id="mrx"><% concatSignatories (map signatorydetails $ documentsignatorylinks) %></span></strong>.</p>
                   </div>
                , <script> var documentid = <% show $ documentid %>; 
                           var issuedone = <% if issuedone then "true" else "false" %>;
                  </script>
                , <script type="text/javascript" src="/js/document-edit.js"/>
                ]
   in showDocumentPageHelper (LinkIssueDoc document) document helper 
           (BS.fromString $ "Avtal: " ++ BS.toString documenttitle)  
      <div>
       <div>

        <% if documentstatus == Preparation
           then 
             <span>
              <strong>1. Dina uppgifter</strong>
              <input name="authorname" type="text" value=(signatoryname documentauthordetails)
                     infotext="Ditt namn"/><br/>
              <input name="authorcompany" type="text" value=(signatorycompany documentauthordetails)
                     infotext="Titel, företag"/><br/>
              <input name="authornumber" type="text" value=(signatorynumber documentauthordetails)
                     infotext="Ditt Orgnr/Persnr"/><br/>
              <input name="authoremail" type="text" value=(signatoryemail documentauthordetails)
                     infotext="Din e-mail"/><br/>

              <strong>2. Din motpart</strong>
              <div id="signatorylist">
               <% map showSignatoryEntryForEdit (if null documentsignatorylinks
                                                 then [emptyDetails] 
                                                 else map signatorydetails documentsignatorylinks) %>
              </div>
              <a onclick="signatoryadd(); return false;" href="#">Lägg till fler</a>

              <hr/>

              <strong>3. Avtal</strong><br/>
              Undertecknas inom <input type="text" name="daystosign" value=documentdaystosign maxlength="2" size="2"/> dagar<br/>
              <input class="bigbutton" type="submit" name="final" value="Underteckna" id="signinvite"/>
              <br/>
              <input class="secbutton" type="submit" name="save" value="Spara som utkast"/>
             </span>
           else
              <div id="signatorylist">
               <% map (showSignatoryEntryStatus document) documentsignatorylinks %>
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
                , <div id="dialog-confirm-sign" title="Underteckna">  
                   {- change "alla parter" to list of people -}
                   <p>Är du säker på att du vill underteckna dokumentet 
                     <strong><% documenttitle document %></strong>?</p>
                   <p>När alla parter undertecknat blir avtalet <strong>juridiskt bindande</strong> och du får det 
                      färdigställda dokumentet via e-post. Efter att du undertecknat får du även 
                      möjlighet att spara dokumentet online via SkrivaPå.
                   </p>
                  </div>
                ]

   in showDocumentPageHelper action document helper 
              (BS.fromString $ "Avtal: " ++ BS.toString(documenttitle document)) $
        if wassigned 
           then <span>Du har redan undertecknat!</span>
           else <span>
                
                 <p>Välkommen <strong><% invitedname %></strong>,</p>
 
                 <p>På vänster sida har du dokumentet <strong><% documenttitle document %></strong> som <strong><% authorname %></strong> har 
                    bjudit in dig att underteckna.
                    Genom att underteckna ingår du ett <strong>juridiskt bindande</strong> avtal. 
                    Vill du underteckna? 
                 </p>

                  {-
                    <p>Om du inte är <strong><% invitedname %></strong> ber vi dig att avvisa avtalet.</p>
                  -}

                  {- Avvisa - gray FIXME -}

                 <input class="bigbutton" type="submit" name="sign" value="Underteckna" id="sign"/>
                 <p>Jag vill veta mer <a href="/about" target="_blank">om SkrivaPå</a>.</p>
                </span>


htmlHeadBodyWrap :: (XMLGenerator m,EmbedAsChild m a {- ,EmbedAsChild m b -})
                 => a
                 -> XMLGenT m (HSX.XMLGenerator.XML m) --b
                 -> XMLGenT m (HSX.XMLGenerator.XML m)
htmlHeadBodyWrap title content =     
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
      <title><% title %></title>
     </head>
     <body>
      <% content %>
     </body>
    </html>

htmlHeadBodyWrapIO title content = do
  let xml = htmlHeadBodyWrap title content
  renderHSPToByteString xml

invitationMail :: Context
               -> BS.ByteString
               -> BS.ByteString
               -> Document
               -> SignatoryLink
               -> MagicHash
               -> IO BS.ByteString
invitationMail (Context {ctxmaybeuser = Just user, ctxhostpart}) 
                  emailaddress personname 
                  document@Document{documenttitle,documentid,documenttimeouttime,documentauthordetails} 
                  signaturelink magichash = 
    let link = ctxhostpart ++ show (LinkSignDoc document signaturelink)
        creatorname = signatoryname documentauthordetails
    in htmlHeadBodyWrapIO documenttitle
     <span>
      <p>Hej <strong><% personname %></strong>,</p>

      <p><strong><% creatorname %></strong> har bjudit in dig att underteckna dokumentet 
         <strong><% documenttitle %></strong>. Så här går du vidare:</p> 

      <p>1. Klicka på länken<br/>
         <a href=link><% link %></a><br/>
         2. Granska dokumentet online<br/>
         3. Underteckna<br/>
      </p>
         <% case documenttimeouttime of
              Just time -> <p>Dokumentet kan undertecknas senast <% show time %>.</p>
              Nothing -> <span/>
          %>

      <% poweredBySkrivaPaPara %>
     </span>

closedMail :: Context
           -> BS.ByteString
           -> BS.ByteString
           -> Document
           -> SignatoryLink
           -> MagicHash
           -> IO BS.ByteString
closedMail (Context {ctxhostpart}) 
              emailaddress personname 
              document@Document{documenttitle,documentid} 
              signaturelink magichash = 
    let link = ctxhostpart ++ show (LinkSignDoc document signaturelink)
    in htmlHeadBodyWrapIO documenttitle 
     <span>
      {- change "alla parter" to list of people -}
      <p>Hej <strong><% personname %></strong>,</p>
      <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats av alla parter 
         och avtalet är nu juridiskt bindande. Nedan bifogas en direktlänk till det
         färdigställda dokumentet och en PDF-kopia.</p>

      <p><a href=link><% link %></a></p>
     
      <% poweredBySkrivaPaPara %>
     </span>


closedMailAuthor :: Context
                 -> BS.ByteString
                 -> BS.ByteString
                 -> Document
                 -> IO BS.ByteString
closedMailAuthor (Context {ctxhostpart}) 
                  emailaddress personname 
                  document@Document{documenttitle,documentid} = 
    let link = ctxhostpart ++ show (LinkIssueDoc document)
    in htmlHeadBodyWrapIO documenttitle
        <span>
         {- change "alla parter" to list of people -}
         <p>Hej <strong><% personname %></strong>,</p>
         <p>Dokumentet <strong><% documenttitle %></strong> har undertecknats av alla parter 
            och avtalet är nu juridiskt bindande. Nedan bifogas en direktlänk till det
            färdigställda dokumentet och en PDF-kopia.</p>
      
         <p><a href=link><% link %></a></p>
     
         <% poweredBySkrivaPaPara %>
        </span>


poweredBySkrivaPaPara :: (XMLGenerator m) => XMLGenT m (HSX.XML m)
poweredBySkrivaPaPara = 
    <p>
     <small>Med vänliga hälsningar<br/>
     <a href="http://skrivapa.se/">SkrivaPå</a></small>
    </p>

