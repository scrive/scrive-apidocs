####
mainpage=
         $upload:noescape()$
         <BR/>
         <div class="box">
         $doclist:noescape()$
         </div>

####

####
pageDocumentList= 
<form method="post" action="">
  <div class="doctablecontainer">
  <table class="doctable" cellspacing="0">
    <col>
    <col>
    <col>
    <col>
    <col>
    <col>
    <thead>
      <tr>
        <td><a href="#" id="all">Alla</a></td>
        <td><span style="padding-left:5px">Status</span></td> 
        <td></td>
        <td>Parter</td>
        <td>Dokument</td>
        <td>Senaste handelse</td>
        <td></td>
      </tr>
    </thead>
    <tfoot>
      <tr>
        <td colspan="7" style="text-align: right; overflow: hidden;">
          <div class="floatleft">
            <input type="submit" class="button" name="archive" value="Radera">
          </div>
          <div class="floatright">
            <img src="/theme/images/status_draft.png"> Utkast
            <img src="/theme/images/status_rejected.png"> Avbrutet
            <img src="/theme/images/status_timeout.png"> Förfallet
            <img src="/theme/images/status_pending.png"> Skickat
            <img src="/theme/images/status_viewed.png"> Granskat av motpart
            <img src="/theme/images/status_signed.png"> Undertecknat
          </div>
          <div class="clearboth"></div>
        </td>
      </tr>
    </tfoot>
    <tbody id="selectable">
      $documents:{document |
      
      <tr class="ui-state-default">
        <td class="tdleft">
          $if(document.isauthor)$ <input type="checkbox" name="doccheck" value="$document.documentid$" class="check"> $endif$
        </td>
        <td>
          <img width="17" height="17" src="$document.statusimage$"> 
          $if(document.anyinvitationundelivered)$  <span style="color:#000000;position:relative;top:-3px">!</span>$endif$
          
        </td>
        <td>
          $if(document.timeoutdaysleft)$  
          <span title="Förfallodatum: $document.timeoutdate$">
            <a href="$document.doclink$">
              ($document.timeoutdaysleft$)
            </a>
          </span> 
          $endif$
        </td>
        <td><a href="$document.doclink$">$document.signatories:{sig | $sig.usvFullname$};separator=", "$</a></td>
        <td><a href="$document.doclink$">$document.documenttitle$</a></td>
        <td><a href="$document.doclink$"><span title="$document.mtime$">$document.mtime$</span></a></td>
        <td class="tdright">
          $if(document.davelink)$ 
          <a href="$document.davelink$">*</a>     
          $endif$
        </td>
      </tr>
      
      }$
    </tbody>
  </table>
  </div>
</form>
     