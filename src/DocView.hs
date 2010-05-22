{-# LANGUAGE IncoherentInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module DocView where
import AppView
import Data.List
import DocState
import HSP
import qualified Data.ByteString.UTF8 as BS
import qualified HSX.XMLGenerator as HSX (XML)
import qualified HSX.XMLGenerator
import User

jquery :: (XMLGenerator m) => [XMLGenT m (HSX.XML m)] 
jquery = [<script src="/js/jquery-1.4.2.min.js"/>,
          <script src="/js/jquery-ui-1.8.custom.min.js"/>]

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
             <a href=("/issue/" ++ show (documentid entry))><% show (title entry) %></a>
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
         <table class="commentlist">
           <% zip entries (cycle [False,True]) %>
         </table>
        %>
{-
seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>
-}

listDocuments :: (EmbedAsChild m c) => c -> GenChildList m
listDocuments documents = asChild documents

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
      <label>Full name:</label><br/> 
      <input name="signatoryname" type="text" value=signatoryname/><br/>
      <label>Company:</label><br/>
      <input name="signatorycompany" type="text" value=signatorycompany/><br/>
      <label>Email:</label><br/>
      <input name="signatoryemail" type="text" value=signatoryemail/><br/>
      <a onclick="signatoryremove(this)" href="#">Remove</a>
    </li>

showSignatoryEntryStatus (SignatoryLink{signatoryname,signatoryemail,maybeseentime}) = 
    <li> 
        <% case maybeseentime of
             Just time -> BS.toString signatoryname ++ " last " ++ show time 
             Nothing -> BS.toString signatoryname
        %>
    </li>

 -- FIXME: add info about date viewed, date signed, send reminder, change email
showFileImages file = 
   [ <img src=("/pages/" ++ show (fileid file) ++ "/" ++ show pageno) width="300"/> |
     pageno <- [1..(length (filejpgpages file))]]

showDocumentBox document = 
    <div id="documentBox">
        <% map showFileImages (files document) %>
    </div>

{- showDocument
  :: (EmbedAsChild m [Char], EmbedAsAttr m (Attr [Char] [Char])) =>
     Document -> XMLGenT m (HSX.XML m)
-}
showDocument
  :: (XMLGenerator m,
      EmbedAsAttr m (Attr [Char] BS.ByteString)) =>
     Document -> XMLGenT m (HSX.XMLGenerator.XML m)
showDocument document =
   <span> <span style="display: none"><% showSignatoryEntryForEdit2 "signatory_template" "" "" "" %></span>
   <form method="post"> 
         <% jquery %>
    <table>
     <tr>
      <td>
       <% showDocumentBox document %>
      </td>
      <td>
       Title:
       <% title document %><br/>
       <div>List of signatories:<br/>

        <% if status document == Preparation
           then <span>
              <ol id="signatorylist">
               <% map showSignatoryEntryForEdit (signatorylinks document) %>
              </ol>
              <a onclick="signatoryadd()" href="#">Add signatory</a>
             </span>
           else
              <ol id="signatorylist">
               <% map showSignatoryEntryStatus (signatorylinks document) %>
              </ol>
                           
         %>
       </div>
      </td>
     </tr>
    </table> 
    <br/>
    <% 
        if status document == ReadyToSign
           then <span/>
           else <span>
                 <input type="submit" value="Update"/>
                 <input class="bigbutton" type="submit" name="final" value="Make it final"/>
                </span>
     %>

     <script type="text/javascript" src="/js/document-edit.js"/>

   </form>
   </span>


showDocumentForSign :: (XMLGenerator m) =>
                       Document -> Bool -> XMLGenT m (HSX.XML m)
showDocumentForSign document wassigned =
   <form method="post"> 
      <% showDocumentBox document %>
      <br/>
      <%
        if wassigned 
           then <span>You have already signed this document!</span>
           else <input type="submit" name="sign" value="Sign!"/>
      %>
   </form>


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
     </head>
     <body>
      <h1>Welcome <% personname %></h1>
      <p><a href=link><% documenttitle %></a></p>
      <p><% creatorname %> prepared documents you should see! 
           To review and sign them click link below:</p>
      <p><% link %></p>
      <p><small>Powered by Skriva Pa</small></p>
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
