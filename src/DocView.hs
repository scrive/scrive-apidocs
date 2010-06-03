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

jquery :: (XMLGenerator m) => [XMLGenT m (HSX.XML m)] 
jquery = [<script src="/js/jquery-1.4.2.min.js" zonk="öåä">öåä</script>,
          <script src="/js/jquery-ui-1.8.custom.min.js"/>]


mkSignDocLink :: String -> DocumentID -> SignatoryLinkID -> String
mkSignDocLink hostpart documentid signaturelinkid =
    hostpart ++ "/sign/" ++ show documentid ++ "/" ++ show signaturelinkid

-- * Convenience Functions

{-
dateStr :: ClockTime -> String
dateStr ct =
  formatCalendarTime
    defaultTimeLocale
    "%a, %B %d, %Y at %H:%M:%S (UTC)"
    (toUTCTime ct)
-}

-- dateStr1 _ = "Today"

-- * Main Implementation

instance (XMLGenerator m) => (EmbedAsChild m (Document, Bool)) where
    asChild (entry, alt) = 
          <%
           <tr class=(if alt then "alt" else "")>
            <td>
             <a href=("/issue/" ++ show (documentid entry))><% title entry %></a>
            </td>
            <td>
             <% show $ documentmtime entry %>
            </td>
            <td>
             <% show (status entry) %>
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
{-
seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>
-}

concatSignatories siglinks = 
    concat $ intersperse ", " $ map (BS.toString . signatoryname) siglinks 

oneDocumentRow document = 
    let link = "/issue/" ++ show (documentid document)
        mk x = <a href=link><% x %></a>
    in
    <tr>
     <td class="tdleft">
      <input type="checkbox"/>
     </td>
     <td><% mk $ concatSignatories (signatorylinks document) %></td>
     <td>skrivaPå</td>
     <td><% mk $ title document %></td>
     <td><% mk $ show (status document) %></td>
     <td class="tdright">*</td>
    </tr>


listDocuments :: (XMLGenerator m) => [Document] -> XMLGenT m (HSX.XML m)
listDocuments documents = 
    <div>
     <br/>
     <table class="doctable" cellspacing="0">
      <thead>
       <tr>
        <td>Alla</td>
        <td>Personer</td>
        <td>Företag</td>
        <td>Dokument</td>
        <td>Status</td>
        <td>*</td>
       </tr>
      </thead>
      <tfoot>
       <tr><td colspan="6">Foot</td></tr>
      </tfoot>
      <tbody>
       <% map oneDocumentRow documents %>
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


{-
showSignatoryEntry
  :: (HSX.XMLGenerator.EmbedAsAttr
        m (HSX.XMLGenerator.Attr [Char] [Char]),
      HSX.XMLGenerator.EmbedAsChild m [Char]) =>
     DocState.SignatoryLink -> HSX.XMLGenerator.GenChildList m
-}
showSignatoryEntryForEdit (SignatoryLink{signatoryname,signatorycompany,signatoryemail}) = 
    showSignatoryEntryForEdit2 "" (BS.toString signatoryname) (BS.toString signatorycompany) 
                                   (BS.toString signatoryemail)

showSignatoryEntryForEdit2 :: (XMLGenerator m) => String -> String -> String -> String -> XMLGenT m (HSX.XML m)
showSignatoryEntryForEdit2 idx signatoryname signatorycompany signatoryemail = 
    <li id=idx>
      <label>Namn på den du vill skriva avtal med</label><br/> 
      <input name="signatoryname" type="text" value=signatoryname/><br/>
      <label>Företag</label><br/>
      <input name="signatorycompany" type="text" value=signatorycompany/><br/>
      <label>Personens email</label><br/>
      <input name="signatoryemail" type="text" value=signatoryemail/><br/>
      <a onclick="return signatoryremove(this);" href="#">Ta bort</a>
      {- days to sign:
         Antal dagar att skriva på -}
    </li>

showSignatoryEntryStatus :: (XMLGenerator m) => SignatoryLink -> XMLGenT m (HSX.XML m)
showSignatoryEntryStatus (SignatoryLink{signatoryname,signatoryemail,maybeseentime,maybesigninfo}) = 
    <li> 
        <b><% signatoryname %></b><br/>
        <% case maybesigninfo of
             Just (SignInfo{signtime}) -> "Signerat " ++ show signtime 
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
        Preparing document...
    </div>


emptyLink = SignatoryLink 
          { signatorylinkid = SignatoryLinkID 0
          , signatoryname = BS.empty
          , signatorycompany = BS.empty
          , signatoryemail = BS.empty
          , maybesignatory = Nothing
          , maybesigninfo  = Nothing
          , maybeseentime  = Nothing
          }
{- showDocument
  :: (EmbedAsChild m [Char], EmbedAsAttr m (Attr [Char] [Char])) =>
     Document -> XMLGenT m (HSX.XML m)
-}
showDocument
  :: (XMLGenerator m,
      EmbedAsAttr m (Attr [Char] BS.ByteString)) =>
     User -> Document -> XMLGenT m (HSX.XMLGenerator.XML m)
showDocument user document =
   let helper = jquery ++ 
                [ <span style="display: none">
                   <% showSignatoryEntryForEdit2 "signatory_template" "" "" "" %>
                  <div id="dialog-confirm-signinvite" title="BEKRÄFTA">
	        <p> När du bekräftat avtalet kommer en automatisk inbjudan att skickas till <span id="mrx">"Mr X"</span>. 
            Avtalet blir juridiskt bindande när båda parter undertecknat och det är först då vi tar betalt. 
            Vi fakturerar månadsvis. Era fakturauppgifter:</p>

            <div class="inlinebox">
            Referens: <% fullname user %> <br/>
            Företag: <% usercompanyname user %> <br/>
            Org nr: <% usercompanynumber user %> <br/>
            Adress: <% userinvoiceaddress user %> <br/>
            Pris: ”20 SEK exkl moms” <br/>
            </div>

           <p>Är du säker på att du vill underteckna avtalet?</p>

{- <span class="ui-icon ui-icon-alert" style="float:left; margin:0 7px 20px 0;"></span>
             These items will be permanently deleted and cannot be recovered. Are you sure?
-}
          </div>
        </span>
                , <script> var documentid = <% show $ documentid document %>; 
                  </script>
                , <script type="text/javascript" src="/js/document-edit.js"/>
                ]
   in showDocumentPageHelper ("/issue/" ++ show (documentid document)) document helper (title document)  
      <div>
       <div>Personer:<br/>

        <% if status document == Preparation
           then <span>
              <ol id="signatorylist">
               <% map showSignatoryEntryForEdit (if null (signatorylinks document)
                                                 then [emptyLink] else signatorylinks document) %>
              </ol>
              <a onclick="return signatoryadd();" href="#">Skapa inbjudan</a>
             </span>
           else
              <ol id="signatorylist">
               <% map showSignatoryEntryStatus (signatorylinks document) %>
              </ol>
                           
         %>
         <hr/>
         <% 
           if (status document==Preparation) 
              then <span>
                    <input class="bigbutton" type="submit" name="final" value="Skriv på och bjud in" id="signinvite"/>
                    <input type="hidden" name="final2" value=""/>
                    <br/>
                    <input class="secbutton" type="submit" name="save" value="Spara till senare"/>
                   </span>
              else <span/>
          %>
       </div>
      </div>

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
       <p class="headline"><% title %> 
           <a href=("/issue/" ++ show (documentid document) ++ "/" ++ BS.toString title ++ ".pdf") target="_blank"> (Open as PDF)</a>
       </p>
       <% content %>
      </td>
     </tr>
    </table> 
   </form>
   <div id="dialog-confirm-sign" title="BEKRÄFTA">
        <p>Är du säker på att du vill underteckna avtalet?</p>

{- <span class="ui-icon ui-icon-alert" style="float:left; margin:0 7px 20px 0;"></span>
             These items will be permanently deleted and cannot be recovered. Are you sure?
-}
   </div>

   </div>


showDocumentForSign :: (XMLGenerator m) =>
                       String -> Document -> BS.ByteString -> BS.ByteString -> Bool -> XMLGenT m (HSX.XML m)
showDocumentForSign action document authorname invitedname wassigned =
   let helper = jquery ++ [ <script type="text/javascript" src="/js/document-edit.js"/>
                , <script> var documentid = <% show $ documentid document %>; 
                  </script>
                          ]
   in showDocumentPageHelper action document helper (title document) $
        if wassigned 
           then <span>Du har redan skrivit på!</span>
           else <span>
                <p>Hej <% invitedname %></p>

                <p>Genom skrivaPå kan du underteckna juridiskt bindande avtal online. Avtalet på vänster sida är avtalet <% title document %> som <% authorname %> har bjudit in dig att underteckna. Du zoomar in genom att klicka på förstoringsglaset. Du undertecknar genom att klicka ”Underteckna” nedan. </p>

                <p>Det är olagligt att underteckna i annans namn och vi anmäler alla misstänkta fall av urkundsförfalskning. Därför ska du under inga som helst omständigheter underteckna om du inte är <% invitedname %>.</p>

{- Avvisa - gray FIXME -}
                <p>Klicka här om du vill veta mer om skrivaPå innan du undertecknar.</p>

                   <input type="hidden" name="sign2" value=""/>
                   <input class="bigbutton" type="submit" name="sign" value="Underteckna" id="sign"/>
                </span>

poweredBySkrivaPaPara :: (XMLGenerator m) => XMLGenT m (HSX.XML m)
poweredBySkrivaPaPara = 
    <p>
     Med vänliga hälsningar<br/>
     <small>Powered by <a href="http://skrivapa.se/">skrivaPå</a></small>
    </p>


invitationMailXml :: (XMLGenerator m) 
                     => Context
                  -> BS.ByteString
                  -> BS.ByteString
                  -> BS.ByteString
                  -> DocumentID
                  -> SignatoryLinkID
                  -> XMLGenT m (HSX.XML m)
invitationMailXml (Context (Just user) hostpart) 
                  emailaddress personname 
                  documenttitle documentid 
                  signaturelinkid = 
    let link = hostpart ++ "/sign/" ++ show documentid ++ "/" ++ show signaturelinkid
        creatorname = BS.toString $ fullname user
    in 
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
     </head>
     <body>
      <p>Hej <% personname %>,</p>
      <p></p>
      <p><% creatorname %> har bjudit in dig att skriva på avtalet <a href=link><% documenttitle %></a>. Klicka på länken för att läsa igenom och skriva på.</p>
      <p><% link %></p>
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
closedMailXml (Context (Just user) hostpart) 
                  emailaddress personname 
                  documenttitle documentid 
                  signaturelinkid = 
    let link = hostpart ++ "/sign/" ++ show documentid ++ "/" ++ show signaturelinkid
        creatorname = BS.toString $ fullname user
    in 
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
     </head>
     <body>
      <p>Hej <% personname %>,</p>
      <p>Avtalet <a href=link><% documenttitle %></a> har signerats av alla parter. Avtalet är nu lagligt bindande. Vi har låst dokumentet så att det inte kan ändras och för att markera detta har vi stämplat det med vårt sigill.</p>
      
      <p>Dokumentet bifogas med detta mail. Om du har ett konto hittar du avtalet i ditt konto under "Dokument". Om du inte har ett konto kan du spara dokumentet genom att <a href=link>klicka här</a>.</p>
     
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
