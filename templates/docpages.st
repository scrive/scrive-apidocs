####    Pages for documents - main part of system
creatingDocumentFrame=   <div id="signStepsContainer">
     <div id="signStepsMenu">
       <div id="step1select" $if(step1)$class="current"$endif$>
         <a href="#" class="step1select clickArea"></a>
         <a href="#" class="step1select step1icon">
         </a>
         <a href="#" class="step1select text">Ladda upp dokument</a>
       </div>

       <div id="step2select" $if(step2)$class="current"$endif$>
         <a href="#" class="step2select clickArea"></a>
         <a href="#" class="step2select step2icon">
         </a>
         <a href="#" class="step2select text">Fyll i uppgifter</a>
       </div>

       <div id="step3select" $if(step3)$class="current"$endif$>
         <a href="#" class="step3select clickArea"></a>
         <a href="#" class="step3select step3icon">
         </a>
         <a href="#" class="step3select text">Underteckna och skicka!</a>
       </div>
                                                
       <div id="signStepsMenuEmptyStep"></div>
     </div>
                                        
     <div id="signStepsTitleRow">
       <span id="signStepsTitleRowTextContainer">
         <span class="title">DOKUMENT:</span>
         <span class="name">$title$ | Transaction #$documentid$</span>
       </span>
                                                
       <div class="signStepsButton $if(submitFormOnNext)$ submitStepFormOnClick $endif$">
         <a href="#nextStep" class="clickArea"></a>
         
         <a href="#nextStep"><span class="icon arrowRight"></span></a><!-- 005 no whitespace here pleace, or one pixel of the button will be unclicable pixel in IE --><a href="#nextStep">
           <span class="text">Nästa steg</span>
         </a>
       </div>
     </div>

     <div id="signStepsBody"> 
        $steps:noescape()$
     </div>
     <div id="signStepsBodyBorderBottom"></div>
     </div>
     </div>

   </div>                       
    
######
uploadPageContent=
       <div id="signStepsBodyUploadBox"> 
       <form action="/d" method="post" enctype="multipart/form-data" class="stepForm"> 
        <span id="signStepsBodyUploadTextContainer"> 
          <span class="header">Välj fil och ladda upp!</span><br>
          <span class="text">OBS! Det går endast att ladda upp .PDF</span> </span> 
          <br>
          <div id="signStepsButtonContainer">
            <div class="signStepsButton" >  
              <a href="#attachFile"><span class="icon arrowUp"></span></a><a href="#attachFile"> 
              <span class="text">Ladda upp fil</span> </a> 
              <input class="multiFileInput" maxlength="1" type="file" name="doc" accept="application/pdf"
                     style="position:relative;z-index:10;cursor:pointer;opacity:0;top:-10;left:-10;font-size:30" 
                     rel=".filelist">
            </div>
          </div>
          <div class="filelist" style="padding-left:10px"></div>
        </div>
       </form>
      </div>
  
####
pageDocumentList= 
     <form method="post" action="">
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
        $if(document.dsvIsauthor)$ <input type="checkbox" name="doccheck" value="$document.dsvId$" class="check"> $endif$
     </td>
     <td>
       <img width="17" height="17" src="$document.dsvStatusimage$"> 
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
                          <img class="pagejpg" src="/pages/$fileid$/$pageno$" width="300">
                      </div> 
                     }$
####
showFileImagesPending= <div class="pagejpga4 pagejpg">
      <img class="waiting" src="/theme/images/wait30trans.gif">
     </div>
####
showFileImagesError=<div class="pagejpga4 pagejpg">
      $normalizelog:noescape()$
     </div> 
####
showDocumentBox=  
    <div id="documentBox">
     <div class="pagejpga4 pagejpg">
      <img class="waiting" src="/theme/images/wait30trans.gif">
     </div>
    </div> 
###
showSignatoryEntryForEdit2= 
 <div id="$idx$" class="signatorybox" alt="Namn på avtalspart">
      <input name="signatoryname" type="text" value="$signatoryname$" autocomplete="off"
             infotext="Namn på motpart"><br>
      <input name="signatorycompany" type="text" value="$signatorycompany$" autocomplete="off"
             infotext="Titel, företag"><br>
      <input name="signatorynumber" type="text" value="$signatorynumber$" autocomplete="off"
             infotext="Orgnr/Persnr"><br>
      <input name="signatoryemail"  type="email" value="$signatoryemail$" autocomplete="off"
             infotext="Personens e-mail"><br>
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
                 $localscript:noescape()$
                </script>
              <div id="signatorylist">
                 $signatorylist:noescape()$
              </div>
       </div>
      </div>

####
showDocumentPageHelper=
    <div class="docview">
     <div style="display: none">
      $helpers:noescape()$
     </div>
      <div class="docviewleft">
       $docbox:noescape()$
      </div>
      <div class="docviewright"> 
       <p><strong>$title$</strong><br>
          <small><a href="$linkissuedocpdf$" target="_blank">Ladda ned PDF</a></small></p>
       $content:noescape()$
      </div>
     <div class="clearboth"></div>
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
                         $reminderMessage:noescape()$
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
                        <input type="text" style="width:170px" name="email" value="$signatoryemail$">
                        <input type="submit" style="width:100px" value="Skicka">
                      </form>     

####
showSignatoryLinkForSign =
                      <div class="signViewBodyRight"> 
                        <div class="signViewBodyIcons"> <a href="#man2" class="man"></a> 
                          <div class="signViewBodyTitle"> <span>Part</span> </div>
                        </div>
                        <div class="signViewBodyRightTextContainer">
                          <div class="text">$signatoryname$</div>
                          <div class="text">$signatoryemail$</div>
                          <div class="text">$signatorycompany$</div>
                          <span class="text">$signatorynumber$</span> 
                        </div>
                      </div>
####

showSignatoryLinkForSign =
<div class="$mainclass$">
 <img src="$status$">

 $if(changeEmailAddress)$
 <span style="color:#000000;position:relative;top:-3px">!</span>
 $endif$

 $if(signatoryname)$
 $signatoryname$<BR>
 $endif$

 $if(signatorycompany)$
 $signatorycompany$<BR>
 $endif$

 $if(signatorynumber)$
 $signatorynumber$<BR>
 $endif$

 $if(signatoryemail)$
 $signatoryemail$<BR>
 $endif$

 <div class="signatoryfields">
 $fields:{field| <div><span class="fieldlabel">$field.fieldlabel$: </span><span class="fieldvalue">$field.fieldvalue$</span></div>}$
 </div>
 
 $message:noescape()$
 <BR>
 $reminderForm:noescape()$
 $changeEmailAddress:noescape()$
</div>

####
pageDocumentForSignHelpers=
                  <script> var documentid = "$documentid$"; 
                  </script>
                  <script type="text/javascript">
                   $localscripts:noescape()$
                  </script>
                 <script src="/js/signatory.js" > </script> 


######
pageDocumentForSignSigned=<div>Du har redan undertecknat!</div>
####
pageDocumentForSignTimedout= <div>Förfallodatum har passerat!</div>
####
pageDocumentForSignButtons=  <div>
			      $if(emailallowed)$
                                 <input class="bigbutton" type="submit" name="sign" value="Underteckna" id="sign" rel="#dialog-confirm-sign">
			      $endif$
			      $if(elegallowed)$
				 <input class="bigbutton cross-button" type="submit" name="signbankid" value="Underteckna with E-Leg" id="signbankid" rel="#dialog-confirm-sign-eleg">
			      $endif$
                                 <input class="bigbutton" type="submit" name="cancel" value="Avvisa" rel="#dialog-confirm-cancel" id="cancel">
                              </div>
####
pageDocumentForSignContent=
    <div id="signStepsContainer"> 
      <div class="localdialogs">
        <div style="display: none">
          $helpers:noescape()$
        </div>
        <form method="post" name="form" action="$action$" id="dialog-confirm-sign" class="overlay">     
          <a href="#" class="close"> </a>                  
          <h2>Underteckna</h2>  
          <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
          <p>När $partyUnsigned:noescape()$ undertecknat blir 
            avtalet <strong>juridiskt bindande</strong> och
            det färdigställda avtalet skickas till din e-post.</p>
          <div class="buttonbox">
            <input type="hidden" name="sign" value="automatic">
            <button class="close button" type="button"> Avbryt </button>
            <button class="submiter button" type="button"> Underteckna </button>
          </div>
        </form>
        <form method="post" name="form" action=action id="dialog-confirm-sign-eleg" class="overlay">     
          <script src="/js/bankid.js"></script>
          <a href="#" class="close"> </a>                  
          <h2>Underteckna with BankID</h2>  
          <p>Är du säker att du vill underteckna dokumentet <strong><% documenttitle document %></strong>?</p>
          <p>När <% partyUnsignedMeAndListString magichash document %> undertecknat blir 
            avtalet <strong>juridiskt bindande</strong> och
            det färdigställda avtalet skickas till din e-post.</p>
          <div class="buttonbox">
            <input type="hidden" name="bankid" value="automatic">
            <input type="hidden" name="signature" value="" id="signature">
            <input type="hidden" name="transactionid" value="" id="transactionid">
            <button class="close button" type="button"> Avbryt </button>
            <button class="bankid button" type="button"> Underteckna with BankID</button>
          </div>
        </form>
        <form method="post" name="form" action="$action$" id="dialog-confirm-cancel" class="overlay">   
          <a href="#" class="close"> </a>     
          <h2>Avvisa</h2>                 
          <p>Är du säker på att du vill avvisa dokumentet <strong>$documenttitle$</strong>?</p>
          <p>När du avvisat kommer vi att skicka ett e-postmeddelande för att meddela <strong>$authorname$</strong>.</p>
          <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
            $rejectMessage:noescape()$
          </div>
          <div class="buttonbox">
            <input type="hidden" name="cancel" value="automatic">
            <button class="close button" type="button"> Avbryt </button>
            <button class="editer button" type="button"> Skriv eget meddelande </button>
            <button class="submiter button" type="button"> Avvisa </button>
          </div> 
        </form> 
      </div>
      <div id="signStepsTitleRow"> <span id="signStepsTitleRowTextContainer"> 
          <span class="title">DOKUMENT:</span> <span class="name">$title$ | 
            Transaction #$docid$</span> </span> </div>
      <div id="signStepsBody"> 
        <div id="signViewBodyLeft" > 
          <span id="signViewBodyLeftTextContainer"> 
            <span class="header">Granska</span><br>
            <span class="text">Vänligen var noga med att granska dokumentet och 
              kontrollera uppgifterna nedan innan du undertecknar.</span>
          </span> 
        </div>
        $signatories:noescape()$
        <div id="signStepsBodyBorderBottom"></div>
      </div>
    </div>
    <div id="signViewNotificationContainer"> 
      <div id="signViewNotificationContainerLeft"> 
        <div class="title">Scrolla ner för att skriva på </div>
        <div class="text">Applications can no longer send you notifications on 
          Facebook. If you like you can start receiving emails from Mocies.</div>
      </div>
      <div id="signViewNotificationContainerRight"> 
        <div class="anchor">
          <a href="#" onclick="jQuery('#signViewNotificationContainer').fadeOut('slow'); return false;">close</a>
        </div>
      </div>
    </div>
    <div id="documentBox">
     <div class="waiting4page"></div>
    </div>
    <div id="signViewBottomBox"> 
      <div id="signViewBottomBoxContainerLeft"> 
        <div id="signViewBottomBoxContainerLeftText"> 
          <input type="checkbox"> <span class="title">Jag intygar...</span> 
          More text should follow
        </div>
      </div>
      <div id="signViewBottomBoxContainerRight"> 
        <input class="button" type="submit" name="sign" value="Underteckna!" id="sign" rel="#dialog-confirm-sign">
        <input class="bigbutton" type="submit" name="cancel" value="Avvisa" rel="#dialog-confirm-cancel" id="cancel">
      </div>
    </div>
    
###

pageDocumentForSignContent_old= 
              <span>
                 <p>Vänligen var noga med att granska dokumentet och kontrollera 
                    uppgifterna nedan innan du undertecknar.</p>   

                $signatories:noescape()$
                $messageoption:noescape()$
                 <span class="localdialogs ">
                  <form method="post" name="form" action="$action$" id="dialog-confirm-sign" class="overlay">     
                     <a class="close"> </a>                  
                     <h2>Underteckna</h2>  
                     <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
                     <p>När $partyUnsigned:noescape()$ undertecknat blir 
                      avtalet <strong>juridiskt bindande</strong> och
                      det färdigställda avtalet skickas till din e-post.</p>
                      <div class="buttonbox">
                      <input type="hidden" name="sign" value="automatic">
                      <button class="close button" type="button"> Avbryt </button>
                      <button class="submiter button" type="button"> Underteckna </button>
                      </div>
                  </form>
                  <form method="post" name="form" action=action id="dialog-confirm-sign-eleg" class="overlay">     
                     <script src="/js/bankid.js"></script>
                     <a class="close"> </a>                  
                     <h2>Underteckna with E-Legitimation</h2>  
                     <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
                     <p>När $partyUnsigned:noescape()$ undertecknat blir 
                      avtalet <strong>juridiskt bindande</strong> och
                      det färdigställda avtalet skickas till din e-post.</p>
                      <div class="buttonbox">
                      <input type="hidden" name="bankid" value="automatic">
                      <input type="hidden" name="signature" value="" id="signature">
                      <input type="hidden" name="transactionid" value="" id="transactionid">
                      <button class="close button" type="button"> Avbryt </button>
                      <button class="bankid button" type="button"> Underteckna with BankID</button>
                      </div>
                 </form>
                 <form method="post" name="form" action="$action$" id="dialog-confirm-cancel" class="overlay">   
                    <a class="close"> </a>     
                       <h2>Avvisa</h2>                 
                    <p>Är du säker på att du vill avvisa dokumentet <strong>$documenttitle$</strong>?</p>
                    <p>När du avvisat kommer vi att skicka ett e-postmeddelande för att meddela <strong>$authorname$</strong>.</p>
                    <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
                     $rejectMessage:noescape()$
                    </div>
                    <div class="buttonbox">
                     <input type="hidden" name="cancel" value="automatic">
                     <button class="close button" type="button"> Avbryt </button>
                     <button class="editer button" type="button"> Skriv eget meddelande </button>
                     <button class="submiter button" type="button"> Avvisa </button>
                    </div> 
                 </form> 
                 </span>
                 
              </span>
####
pageDocumentForAuthorHelpers=
              <span class="templating_insert"> $signatoryEntry:noescape()$</span>,
                  <script> var documentid = "$documentid$"; 
                  </script>
####
pageDocumentForAuthorHelpersLocalDialogs=
    <div class="localdialogs">
      <form method="post" name="form" action="$linkissuedoc$" class="overlay redirectsubmitform" id="dialog-confirm-signinvite" rel="#main-document-form">  
        <a class="close"> </a>
        <h2 id="dialog-title-sign">Underteckna</h2>
        <h2 id="dialog-title-send">Skicka inbjudan</h2>
        <div id="dialog-confirm-text-sign">
          <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
                    
          <p>När du undertecknat kommer en automatisk inbjudan att skickas till 
                                    
            <span class="Xinvited">Invited</span> med e-post.</p>
        </div>

        <div id="dialog-confirm-text-send">
          <p>Du har valt en sekreterarroll och kommer själv inte att underteckna. Är du säker på att du vill skicka en inbjudan att underteckna dokumentet <strong>$documenttitle$</strong> till <span class="Xinvited">Invited</span>?</p>
        </div>
                   
        <div class="buttonbox" >
          <input type="hidden" name="final" value="automatic">
          <button class="close button" type="button"> Avbryt </button>
          <button class="submiter button" type="button"> Underteckna </button>
        </div>
      </form>
      <form method="post" name="form" action=action id="dialog-confirm-sign-eleg" class="overlay">     
        <script src="/js/bankid.js"></script>
        <a class="close"> </a>                  
        <h2>Underteckna with E-Legitimation</h2>  
        <p>CHANGE!!!Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
        <p>När  undertecknat blir 
          avtalet <strong>juridiskt bindande</strong> och
          det färdigställda avtalet skickas till din e-post.</p>
        <div class="buttonbox">
          <input type="hidden" name="bankid" value="automatic">
          <input type="hidden" name="signature" value="" id="signature">
          <input type="hidden" name="transactionid" value="" id="transactionid">
          <button class="close button" type="button"> Avbryt </button>
          <button class="bankid button" type="button"> Underteckna with BankID</button>
        </div>
      </form>  
      <form method="post" name="form" action="" class="overlay" id="edit-invite-text-dialog" >  
        <a class="close"> </a>
        <h2>Hälsningsmeddelande</h2>
        <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
          $invitationMailContent:noescape()$
        </div>
        <div class="buttonbox" >
          <button class="close button" type="button"> Avbryt </button>
          <button class="editer button" type=""> Skriv eget meddelande </button>
          <button class="close button" type="button" id="editing-invite-text-finished"> Ok </button>
        </div>
      </form>  
      <div class="hidden" id="hiddenttimestuffbox">
        <div id="daystosignbox">Undertecknas inom (dagar)
          <BR>
          <input type="text" id="daystosign" name="daystosign" value="$documentdaystosignboxvalue$" maxlength="2" size="2" autocomplete="off">
          <small> <a  class="datetodaystip" rel="#daystosign"> </a> </small>
        </div>
      </div>
    </div>

#####
pageDocumentForAuthorContent_old=
   <div>
       <div id="loading-message" style="display:none">
            Loading pages . . .
       </div>
       <div id="edit-bar">
        $if(preparation)$
             <span>
               <script type="text/javascript">
                 $jscript:noescape()$
               </script>
              <form method="post" name="form" action="$linkissuedoc$" id="main-document-form"> 
              Avsändare<br>
              <div style="margin-bottom: 10px;" id="authordetails">
              <strong><span id="sauthorname">$authorname$<BR></span></strong>
              <span id="sauthorcompany">$authorcompany$<BR></span>
              <span id="sauthornumber">$authornumber$<BR></span>
              <span id="sauthoremail">$authoremail$<BR></span>
              </div>
              Användarroll 
              $if(authorhaslink)$ 
                  
                      <select name="authorrole" id="authorroledropdown">
                                  <option value="signatory">Undertecknare</option>
                                  <option value="secretary">Sekreterare</option>
                      </select>
              $else$
                      <select name="authorrole" id="authorroledropdown">
                                  <option value="secretary">Sekreterare</option>
                                  <option value="signatory">Undertecknare</option>
                      </select> 
              $endif$
              
              <br><br>

              Motpart<br>
              <div id="signatorylist">
               $signatoriesForEdit:noescape()$
              </div>
              <small><a id="addsiglink" onclick="signatoryadd(); return false;" href="#">Lägg till fler</a></small>
              <div style="margin-top: 20px">
              <small><a rel="#edit-invite-text-dialog" id="editinvitetextlink" href="#" style="padding-top:3px">Hälsningsmeddelande</a></small>
              <input type="hidden" id="invitetext" name="invitetext" value="$documentinvitetext$">
              </div>
              <div style="margin-top: 20px"></div>
              <span>
              <input type="checkbox" class="addremovecheckbox flashOnOn" rel="#daystosignbox" location="#datetosigncontainer" oldlocation="#hiddenttimestuffbox" autocomplete="off" value="$if(timetosignset)$on$else$off$endif$" ></input> Välj förfallodatum
                <div id="datetosigncontainer">  </div>
                $if(!timetosignset)$
                <span class="hidden flashMessage" > Varning: Om du väljer ett förfallodatum kan du inte återkalla inbjudan innan datumet förfallit. Detta regleras av avtalslagen.</span>
                $endif$   

              </span>
	      <div>
	        <select name="allowedsignaturetypes" id="allowedbox">
		  <option value="Email+ELeg" $if(emailelegitimation)$ selected="true" $endif$>Email and E-Legitimation (extra charge)</option>
		  <option value="Email" $if(emailonly)$ selected="true" $endif$>Email only</option>
		  <option value="ELeg" $if(elegitimationonly)$ selected="true" $endif$>E-Legitimation (extra charge)</option>
		</select>
              </div>

              <div style="height: 70px;">
              <input class="bigbutton cross-button" type="submit" name="final" value="Underteckna" id="signinvite" rel="#dialog-confirm-signinvite"> <br />
	      <input class="bigbutton cross-button" type="submit" name="signbankid" value="Underteckna with E-Leg" id="signbankid" rel="#dialog-confirm-sign-eleg"> <br />
              <input class="button" type="submit" name="save" value="Spara som utkast">
              </div>
              </form>
              $pageDocumentForAuthorHelpersLocalDialogs:noescape()$
             </span>
        $else$
               <span>
               $if(pending)$
                      <script type="text/javascript" language="Javascript" src="/js/showfields.js">  </script>
                   $if(anyinvitationundelivered)$
                       <p> Adressen 
                          <strong>
                            $undelivered; separator=", "$ 
                             </strong> existerar inte. Kontrollera adressen och försök igen.
                       </p>
                    $endif$
               $endif$
               <script type="text/javascript">
                 $jscript:noescape()$
               </script>
               
              <div id="signatorylist">
                    $signatories:noescape()$
              </div>
              $if(awaitingauthor)$
                  <form method="post" action=""><input class="bigbutton cross-button" type="submit" name="final" value="Underteckna" id="signinvite"></form>
              $endif$
              $if(pending)$
                    $if(!timetosignset)$
                    <span>
                     <input class="button cancel" type="button" name="cancel" value="Återkalla inbjudan"  rel="#cancel-by-author-dialog">    
                     <span class="localdialogs">
                     <form method="post" action="$linkcancel$" class="overlay" id="cancel-by-author-dialog">
                                <a class="close"> </a>
                                <h2> Återkalla inbjudan </h2>
                                <p>Är du säker att du vill återkalla din inbjudan att underteckna dokumentet?
                                <BR>När du återkallat inbjudan kommer nedanstaende meddelande att skickas till dina motparter.
                                </p>
                                <div style="border:1px solid #DDDDDD;padding:3px;margin:5px"> 
                                 $cancelMailContent:noescape()$
                                </div>
                                <div class="buttonbox" >
                                   <button class="close button" type="button"> Avbryt </button>
                                   <button class="editer button" type=""> Skriv eget meddelande </button>
                                   <button class="submiter button" type="button"> Återkalla inbjudan</button>
                                </div>
                          </form>
                       </span>
                       </span>
                    $else$
                       <span>Du kan inte återkalla inbjudan före förfallodatum.</span>
                    $endif$   
            $endif$      
            $if(canberestarted)$
              $restartForm:noescape()$
            $endif$ 
        $endif$
       </div>
      </div>

####

#####
pageDocumentForAuthorContent=
   <div style="display: none">
      $helpers:noescape()$
   </div>
   <div id="signStepsContainer">
     <div id="signStepsMenu">
       <div id="step1select">
         <a href="#" class="step1select clickArea"></a>
         <a href="#" class="step1select step1icon">
	 </a>
	 <a href="#" class="step1select text">Ladda upp dokument</a>
       </div>

       <div id="step2select" class="current">
	 <a href="#" class="step2select clickArea"></a>
	 <a href="#" class="step2select step2icon">
	 </a>
	 <a href="#" class="step2select text">Fyll i uppgifter</a>
       </div>

       <div id="step3select">
	 <a href="#" class="step3select clickArea"></a>
	 <a href="#" class="step3select step3icon">
	 </a>
	 <a href="#" class="step3select text">Underteckna och skicka!</a>
       </div>
						
       <div id="signStepsMenuEmptyStep"></div>
     </div>
					
     <div id="signStepsTitleRow">
       <span id="signStepsTitleRowTextContainer">
	 <span class="title">DOKUMENT:</span>
	 <span class="name">$title$ | Transaction #$documentid$</span>
	 <!-- <span class="changeName"><a href="#changeName">BYT TITEL</a></span> -->
       </span>
						
       <div class="signStepsButton">
	 <a href="#nextStep" class="clickArea"></a>
	 
	 <a href="#nextStep"><span class="icon arrowRight"></span></a><!-- 005 no whitespace here pleace, or one pixel of the button will be unclicable pixel in IE --><a href="#nextStep">
	   <span class="text">Nästa steg</span>
	 </a>
       </div>
     </div>
			
     <div id="signStepsContentStack">

     <form action="" method="post">
       <input type="hidden" name="sigid" value="">
       <input type="hidden" name="allowedsignaturetypes" value="Email">

     <div id="signStep2Content">
       <div id="signStepsBody">
	 
         <div id="signStepsBodyLeft">
	   <div id="people">
	     <div class="signStepsBodyTitle">
	       <span>PERSONER</span>
	     </div>
	     
	     <div id="peopleList" class="signStepsBodyForms">
               <ol>
	         <li><a href="#">aaa 1</a>
	         <li><a href="#">aaa 2</a>
	         <li><a href="#">aaa 3</a>
               </ol>
	     </div>
	     
	     <!--<div class="signStepsBodyAddField partCount">
	              <span>Visa</span>
	              <a href="">11-20</a>,
	              <a href="">21-30</a>
	     </div>
             -->
	   </div>
         </div>
	 
         <div id="signStepsBodyRight">
	   <div class="signStepsBodyTitle">
	     <span>PART 3</span>
	   </div>
	   
	   <!-- 008 change/new -->
	   <div class="signStepsBodyIcons">
	     <a href="#man2" class="man"></a>
	     <a href="#briefcase2" class="briefcase"></a>
	     <a href="#hand2" class="hand"></a>
	   </div>
	   
	   <!-- 013 changed -->
	   <div class="signStepsBodyForms" id="peopleDetails">
	     <div>
	       <input type="text" infotext="För- & efternamn" name="signatoryname" autocomplete="off" value="1">
	       <input type="text" infotext="Ev. Företagsnamn" name="signatorycompany" autocomplete="off">
	       
	       <input type="email" infotext="E-post" name="signatoryemail" autocomplete="off">
	       <input type="text" infotext="Org.-nr / Personnr." name="signatorynumber" autocomplete="off">
	     </div>
	     <div>
	       <input type="text" infotext="För- & efternamn" name="signatoryname" autocomplete="off" value="2">
	       <input type="text" infotext="Ev. Företagsnamn" name="signatorycompany" autocomplete="off">
	       
	       <input type="email" infotext="E-post" name="signatoryemail" autocomplete="off">
	       <input type="text" infotext="Org.-nr / Personnr." name="signatorynumber" autocomplete="off">
	     </div>
	     <div>
	       <input type="text" infotext="För- & efternamn" name="signatoryname" autocomplete="off" value="3">
	       <input type="text" infotext="Ev. Företagsnamn" name="signatorycompany" autocomplete="off">
	       
	       <input type="email" infotext="E-post" name="signatoryemail" autocomplete="off">
	       <input type="text" infotext="Org.-nr / Personnr." name="signatorynumber" autocomplete="off">
	     </div>
	   </div>
	   
	   <div class="signStepsBodyAddField">
	     <a href="#plus2" class="plus"></a>
	     <a href="#minus2" class="minus"></a>
	     <p>Fält</p>
	   </div>
         </div>
	 
         <div id="signStepsBodyFunctions">
	   <div id="signStepsBodyFunctionsBox">
	     <ul>
	       <!-- 008 changed/new -->
	       <li><a href="#add" class="plus" id="addSignatory">LÄGG TILL PERSON</a></li>
	       <li><a href="#remove" class="minus" id="delSignatory">TA BORT PERSON</a></li>
	       <!-- <li><a href="#save" class="floppy">SPARA SOM MALL</a></li> -->
	       <!-- <li><a href="#download" class="arrowDown">LADDA NED SOM PDF</a></li> -->
	     </ul>
	   </div>
         </div>
       </div>
     </div>

     <div id="signStep3Content" style="display: none">
       <div id="signStepsBody">
	   
	 <div id="signStepsBodyLeft" class="step3">
	   <div class="signStepsBodyTitle">
	     <span>DOKUMENTHANTERING</span>

	   </div>
	   <div class="options">
	     <p>
               <span>
               <input type="checkbox" class="addremovecheckbox flashOnOn" rel="#daystosignbox" location="#datetosigncontainer" oldlocation="#hiddenttimestuffbox" autocomplete="off" value="$if(timetosignset)$on$else$off$endif$" ></input>Aktivera förfallodatum
                <div id="datetosigncontainer" style="padding-left:15px">  </div>
                <span class="hidden flashMessage" > Varning: Om du väljer ett förfallodatum kan du inte återkalla inbjudan innan datumet förfallit. Detta regleras av avtalslagen.</span>
              </span>
	    </p>
	     
	     <p>
	       <input type="checkbox" id="b" />

	       <label for="b">Aktivera automatisk påminnelse</label>
	     </p>

             <!-- 
	        <p class="signStepsBodySaveMethods">
		  <label for="c">Lagring</label> 
		  <select id="c">
		    <option value="1">Standard</option>
		    <option value="2">Trustweaver</option>

		    <option value="3">Option 3</option>
		  </select>
	        </p>
                -->
	   </div>
	 </div>
	 
	 
	 <div id="signStepsBodyRight" class="step3">
	   <div class="border left"></div>
	   <div class="border right"></div>

	   
	   <div class="signStepsBodyTitle">
	     <span>INBJUDAN</span>
	   </div>
	   
	   <div class="signStepsBodyInvation">
              <small><a rel="#edit-invite-text-dialog" id="editinvitetextlink" href="#" style="padding-top:3px">JUSTERA HÄLSNINGSMEDDELANDE</a></small>
              <input type="hidden" id="invitetext" name="invitetext" value="$documentinvitetext$">
	   </div>
	   
	   <!-- 015 new -->

	   <div id="signStepsBodyUploadBox" class="step3">

	     <span id="signStepsBodyUploadTextContainer">
	       <span class="text">Bifoga dokument med inbjudan</span>
	     </span>
	     <br />
	        
	     <div id="signStepsButtonContainer">
	       <div class="signStepsButton">
		 <a href="#attachFile" class="clickArea"></a>
		 
		 <a href="#attachFile"><span class="icon arrowUp"></span></a><a href="#attachFile">
                      
		   <span class="text">Ladda upp fil</span>
		 </a>
	       </div>
	     </div>
	   </div>

	 </div>
	 <div id="signStepsBodyFunctions" class="step3">
	   <div class="signStepsBodyTitle">
	     <span>SIGNERING</span>
	   </div>
	   <div class="signStepsBodyFunctionsBox options">
             <!--
	        <p>
		  <label for="d">Metod</label> 
		  <select id="d">
		    <option value="1">e-post</option>
		    <option value="2">e-legitimation</option>
		    <option value="3">Option 3</option>
		  </select>
	        </p>
	        -->
	     <!-- 013 changed sign button tag -->
	     <input type="submit" value="" name="final" class="buttonSignLarge">
	   </div>
	 </div>
	 
       </div>			
       </form>
       $pageDocumentForAuthorHelpersLocalDialogs()$
     </div>
     </div>
						
     <!-- span>signStepsBorderBottom går upp till titelRow om man tar bort denna text. förslag på lösning? Lösning: lägg till float:left på signStepsBodyBorderBottom /johan  </span -->
     <div id="signStepsBodyBorderBottom"></div>
   </div>			
    <div id="documentBox">
     <div class="waiting4page"></div>
    </div>
####
