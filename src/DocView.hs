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
showSignatoryEntry (SignatoryLink{signatoryname,signatoryemail}) = 
    <% <li> 
      <input name="signatoryname" type="text" value=signatoryname/><br/>
      <input name="signatoryemail" type="text" value=signatoryemail/><br/>
      <div class="dragableBox">SIGNATURE</div>
      <a onclick="signatoryremove(this)" href="#">Remove</a>
      </li>
    %>


fff file = 
   <img src=link width="300"/>
   where link = "/pages/" ++ show (fileid file)

{- showDocument
  :: (EmbedAsChild m [Char], EmbedAsAttr m (Attr [Char] [Char])) =>
     Document -> XMLGenT m (HSX.XML m)
-}
showDocument
  :: (XMLGenerator m,
      EmbedAsAttr m (Attr [Char] BS.ByteString)) =>
     Document -> XMLGenT m (HSX.XMLGenerator.XML m)
showDocument document =
   <form method="post"> 
         <% jquery %>
    <table>
     <tr>
      <td><div id="dropBox">
        <% map fff (files document) %>
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
                => Context
             -> BS.ByteString
             -> BS.ByteString
             -> BS.ByteString
             -> DocumentID
             -> SignatoryLinkID
             -> XMLGenT m (HSX.XML m)
mailToPerson (Context (Just user) hostpart) emailaddress personname 
             documenttitle documentid signaturelinkid = 
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
