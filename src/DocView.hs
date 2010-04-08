{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module DocView where
import AppView
import Data.List
import DocState
import HSP
import qualified Data.ByteString.UTF8 as BSC
import qualified HSX.XMLGenerator as HSX (XML)

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
             <a href=("/issue/" ++ show (documentid entry))><% show entry %></a>
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

listDocuments documents = asChild documents

showFile file = <li><% BSC.toString file %></li>

showSignatory sig = <li><% show sig %></li>

showDocument document =
   <form method="post"> 
    <table>
     <tr>
      <td><img src="/theme/images/kontrakt.jpg" width="300"/>
       <ol>
        <% map showFile (files document) %>
       </ol>
      </td>
      <td>
       <div>Status: preparation</div> 
       <hr/>
       <div>Fields (drag end drop on the view)</div>
       <hr/>
       <div><p>Author: <% show $ author document %></p></div>
       <hr/>
       <div>List of signatories:<br/>
        <textarea name="signatories">
         <% intersperse "\n" $ map show (signatories document) %>
        </textarea>
       </div>
      </td>
     </tr>
    </table> 
    <br/>
    <% 
        if final document
           then <span/>
           else <span><input type="submit" value="Update"/>
                 <input type="submit" name="final" value="Make it final"/></span>
     %>
   </form>

showDocumentForSign document =
   <form method="post"> 
      <img src="/theme/images/kontrakt.jpg" width="300"/>
      <br/>
      <input type="submit" name="sign" value="Sign!"/>
   </form>


mailToPerson :: (XMLGenerator m) 
                => String 
             -> String
             -> String
             -> DocumentID
             -> XMLGenT m (HSX.XML m)
mailToPerson emailaddress personname documenttitle documentid = 
    <html>
     <head>
     </head>
     <body>
      <h1>Please sign documents you requested!</h1>
      <p>Click <a href=("http://localhost:8000/sign/" ++ show documentid)><% documenttitle %></a> to sign document.</p>
      <p><small>Powered by Skriva Pa</small></p>
     </body>
    </html>
