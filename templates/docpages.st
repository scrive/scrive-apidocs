####    Pages for documents - main part of system
pageDocumentList= 
     <form method="post" action="">
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
        <td></td> 
        <td></td>
        <td>Motparter</td>
        <td>Dokument</td>
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
      <tbody id="selectable">
       $documents:{document |

     <tr class="ui-state-default">
     <td class="tdleft">
      <input type="checkbox" name="doccheck" value="$document.dsvId$" class="check" />
     </td>
     <td>
       <img width="17" height="17" src="$document.dsvStatusimage$"/> 
         $if(document.dsvAnyinvitationundelivered)$  <span style="color:#000000;position:relative;top:-3px">!</span>$endif$

     </td>
     <td>
     $if(document.dsvTimeoutdaysleft)$  
      <span title="Förfallodatum: $document.dsvTimeoutdate$">
        <a href="$document.dsvDoclink$">
        ($document.dsvTimeoutdaysleft$)
        </a>
      </span> 
     $endif$
     </td>
     <td><a href="$document.dsvDoclink$">$document.dsvSignatories:{sig | $sig.usvFullname$};separator=", "$</a></td>
     <td><a href="$document.dsvDoclink$">$document.dsvTitle$</a></td>
     <td><a href="$document.dsvDoclink$"><span title="$document.dsvMtime$">$document.dsvMtime$</span></a></td>
     <td class="tdright">
     $if(document.dsvDavelink)$ 
      <a href="$document.dsvDavelink$">*</a>     
     $endif$
     </td>
     </tr>

       }$
      </tbody>
     </table>
     </form>

####
showFileImagesReady= $pages:{pageno| 
                      <div id="page$pageno$" class="pagediv">
                          <img class="pagejpg" src="/pages/$fileid$/$pageno$" width="300" />
                      </div> 
                     }$
####
showFileImagesPending= <div class="pagejpga4 pagejpg">
      <img class="waiting" src="/theme/images/wait30trans.gif"/>
     </div>
####
showFileImagesError=<div class="pagejpga4 pagejpg">
      $normalizelog$
     </div> 
####
