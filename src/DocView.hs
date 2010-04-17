{-# LANGUAGE IncoherentInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module DocView where
import AppView
import Data.List
import DocState
import HSP
import qualified Data.ByteString.UTF8 as BSC
import qualified HSX.XMLGenerator as HSX (XML)
import qualified HSX.XMLGenerator

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
             2010-01-01 12:43
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
     BSC.ByteString -> XMLGenT m (HSX.XML m)
showFile file = <li><% BSC.toString file %></li>

showSignatory
  :: (EmbedAsChild m String, Show a) => a -> XMLGenT m (HSX.XML m)
showSignatory sig = <li><% show sig %></li>


showSignatoryEntry
  :: (HSX.XMLGenerator.EmbedAsAttr
        m (HSX.XMLGenerator.Attr [Char] [Char]),
      HSX.XMLGenerator.EmbedAsChild m [Char]) =>
     DocState.SignatoryLink -> HSX.XMLGenerator.GenChildList m
showSignatoryEntry (SignatoryLink{signatoryname,signatoryemail}) = 
    <% <li> 
      <input name="signatoryname" type="text" value=signatoryname/><br/>
      <input name="signatoryemail" type="text" value=signatoryemail/><br/>
      <div class="dragableBox">SIGNATURE</div>
      <a onclick="signatoryremove(this)" href="#">Remove</a>
      </li>
    %>


showDocument
  :: (EmbedAsChild m [Char], EmbedAsAttr m (Attr [Char] [Char])) =>
     Document -> XMLGenT m (HSX.XML m)
showDocument document =
   <form method="post"> 
    <table>
     <tr>
      <td><div id="dropBox"><img src="/theme/images/kontrakt.jpg" width="300"/>
       <ol>
        <% map showFile (files document) %>
       </ol>
       </div>
      </td>
      <td>
       <div>List of signatories:<br/>
        <ol id="signatorylist">
         <% map showSignatoryEntry (signatorylinks document) %>
        </ol>
        <a onclick="signatoryadd()" href="#">Add signatory</a>
       </div>
      </td>
     </tr>
    </table> 
    <br/>
    <% 
        if status document == ReadyToSign
           then <span/>
           else <span><input type="submit" value="Update"/>
                 <input type="submit" name="final" value="Make it final"/></span>
     %>

     <script type="text/javascript" src="/js/drag-drop-custom.js"/>
     <script type="text/javascript" src="/js/document-edit.js"/>

   </form>

showDocumentForSign
  :: (XMLGenerator m) =>
     t -> Bool -> XMLGenT m (HSX.XML m)
showDocumentForSign document wassigned=
   <form method="post"> 
      <img src="/theme/images/kontrakt.jpg" width="300"/>
      <br/>
      <%
        if wassigned 
           then <span>You have already signed this document!</span>
           else <input type="submit" name="sign" value="Sign!"/>
      %>
   </form>


mailToPerson :: (XMLGenerator m) 
                => String 
             -> String
             -> String
             -> DocumentID
             -> SignatoryLinkID
             -> XMLGenT m (HSX.XML m)
mailToPerson emailaddress personname documenttitle documentid signaturelinkid = 
    let link = "http://localhost:8000/sign/" ++ show documentid ++ "/" ++ show signaturelinkid
    in 
    <html>
     <head>
     </head>
     <body>
      <h1>Welcome <% personname %></h1>
      <p><a href=link><% documenttitle %></a></p>
      <p>Document is ready! To review and sign click this link:</p>
      <p><% link %></p>
      <p><small>Powered by Skriva Pa</small></p>
     </body>
    </html>
