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
       <div>Status: preparation</div> 
       <hr/>
       <div id="leftColumn">
                        Drag and drop to place whre it should be:<br/>
			<div class="dragableBox" id="box1">SIGNATURE</div>

			<div class="dragableBox" id="box2">INITIALS</div>
		</div>
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
        if status document == ReadyToSign
           then <span/>
           else <span><input type="submit" value="Update"/>
                 <input type="submit" name="final" value="Make it final"/></span>
     %>

     <script type="text/javascript" src="/js/drag-drop-custom.js"></script>
 <script type="text/javascript">
 function dropItems(idOfDraggedItem,targetId,x,y)
 {
	var targetObj = document.getElementById(targetId);
	var sourceObj = document.getElementById(idOfDraggedItem);
		
        var html = targetObj.innerHTML;
	if(html.length!=0)html = html + '&lt;br&gt;';
	html = html + 'Item "' + document.getElementById(idOfDraggedItem).innerHTML + '" dropped';
	targetObj.innerHTML = html;
 }

 var dragDropObj = new DHTMLgoodies_dragDrop();
 dragDropObj.addSource('box1',true);
 dragDropObj.addSource('box2',true);
 dragDropObj.addTarget('dropBox','dropItems');
 dragDropObj.init();
 </script>

   </form>

showDocumentForSign
  :: (EmbedAsAttr m (Attr [Char] [Char])) =>
     t -> XMLGenT m (HSX.XML m)
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
      <h1>Welcome <% personname %></h1>
      <p><a href=("http://localhost:8000/sign/" ++ show documentid)><% documenttitle %></a></p>
      <p>Document is ready! To review and sign click this link:</p>
      <p><% "http://localhost:8000/sign/" ++ show documentid</a></p>
      <p><small>Powered by Skriva Pa</small></p>
     </body>
    </html>
