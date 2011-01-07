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
showDocumentBox=  
    <div id="documentBox">
     <div class="pagejpga4 pagejpg">
      <img class="waiting" src="/theme/images/wait30trans.gif"/>
     </div>
    </div> 
###
showSignatoryEntryForEdit2= 
 <div id="$idx$" class="signatorybox" alt="Namn på avtalspart">
      <input name="signatoryname" type="text" value="$signatoryname$" autocomplete="off"
             infotext="Namn på motpart"/><br/>
      <input name="signatorycompany" type="text" value="$signatorycompany$" autocomplete="off"
             infotext="Titel, företag"/><br/>
      <input name="signatorynumber" type="text" value="$signatorynumber$" autocomplete="off"
             infotext="Orgnr/Persnr"/><br/>
      <input name="signatoryemail"  type="email" value="$signatoryemail$" autocomplete="off"
             infotext="Personens e-mail"/><br/>
      <small><a onclick="return signatoryremove(this.parentNode);" href="#">Ta bort</a></small>
    </div>
####
####
pageDocumentForViewerHelpers=
                  $signatoryentryforedit$
                  <script> var documentid = "$documentid$";  </script>
####
pageDocumentForViewerContent=  
    <div>
       <div id="loading-message" style="display:none">
            Loading pages . . .
       </div>
       <div id="edit-bar">
                <script type="text/javascript" language="Javascript" src="/js/showfields.js"> 
                </script>
                <script type="text/javascript">
                 $localscript$
                </script>
              <div id="signatorylist">
                 $signatorylist$
              </div>
       </div>
      </div>

####
showDocumentPageHelper=
    <div class="docview">
     <div style="display: none">
      $helpers$
     </div>
      <div class="docviewleft">
       $docbox$
      </div>
      <div class="docviewright"> 
       <p><strong>$title$</strong><br/>
          <small><a href="$linkissuedocpdf$" target="_blank">Ladda ned PDF</a></small></p>
       $content$
      </div>
     <div class="clearboth"/>
    </div> 
####
signatoryMessageSigned=Undertecknat $date$
###
signatoryMessageTimedout=Förfallodatum har passerat
###
signatoryMessageSeen=Granskat $date$
###
signatoryMessageNotSigned=Har ej undertecknat
###
reminderTextSigned=Skicka dokumentet igen
###
reminderTextNotSigned=Skicka påminnelse
###
reminderSenderTextSigned=Skicka
###
reminderSenderTextNotSigned=Skicka påminnelse              
###
reminderEditorText=Skriv eget meddelande                          
###       
reminderForm = <span>
                      <a style="cursor:pointer" class="prepareToSendReminderMail" rel="#siglnk$signatorylinkid$">  $reminderText$  </a>
                      <form class="overlay" action="$linkremind$" method="POST" title="$reminderDialogTitle$" width="600" height="$dialogHeight$" id="siglnk$signatorylinkid$">
                       <a class="close"> </a>
                       <h2> $reminderDialogTitle$ </h2>
                       <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
                         $reminderMessage$
                       </div>
                       <div class="buttonbox">
                       <button class="close button" type="button"> Avbryt </button>
                       <button class="editer button" type="button"> $reminderEditorText$ </button>
                       <button class="submiter button" type="button"> $reminderSenderText$ </button>
                       </div>
                      </form>     
                    </span>       
####
changeEmailAddress =  <a style="cursor:pointer" class="replacebynextonclick"> Skicka inbjudan till ny adress  </a>
                      <form action="$linkchangeemail$" method="POST" style="display:none">
                        <input type="text" style="width:170px" name="email" value="$signatoryemail$"/>
                        <input type="submit" style="width:100px" value="Skicka"/>
                      </form>     

####
showSignatoryLinkForSign =
<div class="$mainclass$">
 <img src="$status$"/>

 $if(changeEmailAddress)$
 <span style="color:#000000;position:relative;top:-3px">!</span>
 $endif$

 $if(signatoryname)$
 $signatoryname$<BR/>
 $endif$

 $if(signatorycompany)$
 $signatorycompany$<BR/>
 $endif$

 $if(signatorynumber)$
 $signatorynumber$<BR/>
 $endif$

 $if(signatoryemail)$
 $signatoryemail$<BR/>
 $endif$

 <div class="signatoryfields">
 $fields:{field| <div><span class="fieldlabel">$field.fieldlabel$: </span><span class="fieldvalue">$field.fieldvalue$</span></div>}$
 </div>
 
 $message$
 <BR/>
 $reminderForm$
 $changeEmailAddress$
</div>

####
