####    Pages for documents - main part of system
creatingDocumentFrame=   
<div id="signStepsContainer">
  
  <div id="signStepsTitleRow">
    <span id="signStepsTitleRowTextContainer">
      $if(title)$
      <span class="title">DOKUMENT:</span>
      <span class="name">$title$</span> 
      <span class="title"> | </span> 
      <span class="title">  Transaktion #$documentid$ </span> 
      $endif$
    </span>
    
    <div class="signStepsButton $if(submitFormOnNext)$ submitStepFormOnClick $endif$">
      <a href="#nextStep" class="clickArea"> </a>
      <a href="#nextStep"><span class="icon arrowRight"></span></a>
      <!-- 005 no whitespace here pleace, or one pixel of the button
      will be unclicable pixel in IE -->
      <a href="#nextStep">
        <span class="text">Nästa steg</span>
      </a>
    </div>
  </div>


  <div id="signStepsMenu">
    <div id="step1select" $if(step1)$class="current"$endif$>
      <span class="step1select clickArea"> </span>
      <span class="step1select step1icon"> </span>
      <span class="step1select text">Ladda upp dokument</span>
    </div>
    
    <div id="step2select" $if(step2)$class="current"$endif$>
      <span class="step2select clickArea"> </span>
      <span class="step2select step2icon"> </span>
      <span class="step2select text">Fyll i uppgifter</span>
    </div>
    
    <div id="step3select" $if(step3)$class="current"$endif$>
      <span class="step3select clickArea"> </span>
      <span class="step3select step3icon"> </span>
      <span class="step3select text">Underteckna och skicka!</span>
    </div>
    
    <div id="signStepsMenuEmptyStep"></div>
  </div>
  
  <div id="signStepsBody"> 
    $steps:noescape()$
  </div>
  <div id="signStepsBodyBorderBottom"></div>
</div>            
    
######
uploadPageContent=
<div id="signStepsBodyUploadBox"> 
  <form action="/d" method="post" enctype="multipart/form-data"
        class="stepForm"> 
    <div>
      <span id="signStepsBodyUploadTextContainer"> 
        <span class="header">Välj fil och ladda upp!</span><br>
        <span class="text">OBS! Det går endast att ladda upp filer med formatet .PDF</span> 
      </span> 
      <br>
      <div id="signStepsButtonContainer">
        <div class="signStepsButton" >  
          <a href="#attachFile"><span class="icon arrowUp"></span></a>
          <a href="#attachFile"> <span class="text">Ladda upp fil</span> </a> 
          <input class="multiFileInput submitOnUpload" maxlength="1" type="file" name="doc" accept="application/pdf"
                 style="position:relative;z-index:10;cursor:pointer;float:right;top:-15;left:10;font-size:xx-large;cursor:pointer;cursor:hand
                        -ms-filter: 'progid:DXImageTransform.Microsoft.Alpha(Opacity=0)';
                        filter: progid:DXImageTransform.Microsoft.Alpha(Opacity=0);
                        opacity: 0;" 
                 rel=".filelist" />
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
showFileImagesPending= 
<div class="pagejpga4 pagejpg">
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
<span class="templating_insert"> $signatoryEntry:noescape()$</span>,
<script type="text/javascript"> 
  var documentid = "$documentid$"; 
</script>
###
pageDocumentForViewerContent = 
  <div class="localdialogs">
    <div style="display: none">
      $helpers:noescape()$
    </div>
  </div>
<div id="signStepsContainer"> 
  <div id="signStepsTitleRow"> 
     <span id="signStepsTitleRowTextContainer"> 
      <span class="title">DOKUMENT:</span>
      <span class="name">$documenttitle$ |</span>
      <span class="title"> Transaktion #$documentid$ </span> 
     </span>
     <div class="downloadButton" >
        <a href="$linkissuedocpdf$" target="_blank">
         <span class="icon arrowDownIcon"></span>
         <span class="text">Ladda ned PDF</span>
       </a>
   </div>
  </div>
  <div id="signStepsBody" class="forauthor"> 
    <div id="signViewBodyLeft" > 
      <span id="signViewBodyLeftTextContainer"> 
          $documentinfotext:noescape()$
      </span> 

  <div> 
   $if(awaitingauthor)$
      <script type="text/javascript" src="/js/signatory.js" ></script>
   $$
   $elseif(pending)$
      <script type="text/javascript" src="/js/signatory.js" ></script>
   $endif$      
  </div>

    </div>
    $signatories:noescape()$

    <div id="signStepsBodyBorderBottom"></div>
  </div>
</div>

<div id="documentBox">
  <script type="text/javascript">
    var docstate = $docstate:noescape()$;
  </script>

  <!-- 
    I need to turn this stuff off since it brakes js now. TODO!!!!  
    <script type="text/javascript" src="/js/signatory.js"></script>
  -->
  <div class="waiting4page"></div>
</div>

####
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
reminderForm = 
<span>
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
changeEmailAddress =  
<a style="cursor:pointer" class="replacebynextonclick"> Skicka inbjudan till ny adress  </a>
<form action="$linkchangeemail$" method="POST" style="display:none">
  <input type="text" style="width:140px" name="email" value="$signatoryemail$">
  <input type="submit" style="width:100px" value="Skicka">
</form>     

####
showSignatoryLinkForSign =
<div class="signViewBodyRight"> 
  <div class="signViewBodyRightTextContainer"> 
   <img src="$status$" style="position:relative;top:3px;"> 
   $if(changeEmailAddress)$ <span style="color:#000000;position:relative;left:-4px;">!</span>  $endif$
   $if(signatoryname)$<span class="text"><strong>$signatoryname$</strong></span>$endif$
   $if(signatoryemail)$<div class="text">$signatoryemail$</div>$endif$
   $if(signatorycompany)$<div class="text">$signatorycompany$</div>$endif$
   $if(signatorynumber)$<div class="text">$signatorynumber$</div>$endif$
   <span class="signatoryfields">
     $fields:{field| <div class="text"><span class="fieldlabel">$field.fieldlabel$: </span><span class="fieldvalue">$field.fieldvalue$</span></div>}$
   </span>
   <span class="signViewBodyForms"></span>
   $if(message)$<div class="text">$message:noescape()$</div>$endif$   
   $if(reminderForm)$<div class="text">$reminderForm:noescape()$</div>$endif$
   $if(changeEmailAddress)$<div class="text">$changeEmailAddress:noescape()$</div>$endif$   
  </div>
  
  
</div>
####
####
pageDocumentForSignHelpers=
<script type="text/javascript"> var documentid = "$documentid$"; 
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
pageDocumentForSignButtons=  
<div>
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
      <script type="text/javascript" src="/js/bankid.js"></script>
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
<div id="signStepsContainer"> 
  <div id="signStepsTitleRow"> <span id="signStepsTitleRowTextContainer"> 
      <span class="title">DOKUMENT:</span> <span class="name">$title$ | </span> <span class="title"> Transaktion #$docid$ </span> 
     </span>
     <div class="downloadButton" >
        <a href="$linkissuedocpdf$" target="_blank">
         <span class="icon arrowDownIcon"></span>
         <span class="text">Ladda ned PDF</span>
       </a>
   </div>
  </div>
  <div id="signStepsBody"> 
    <div id="signViewBodyLeft" > 
      <span id="signViewBodyLeftTextContainer"> 
        $documentinfotext:noescape()$
      </span> 
    </div>
    $signatories:noescape()$
    <div id="signStepsBodyBorderBottom"></div>
  </div>
</div>
<div id="signViewNotificationContainer"> 
  <div id="signViewNotificationContainerLeft"> 
    <div class="title">Dokumentet skrivs på längst ner på sidan. </div>
    
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
      <input type="checkbox" class="signGuard" autocomplete="off"> Jag intygar att jag har granskat och förstått innehållet i dokumentet. Jag intygar även att samtliga parter är korrekta och att jag har befogenhet att skriva på.
    </div>
  </div>
  <div id="signViewBottomBoxContainerRight"> 
    <input class="button" type="submit" name="sign" value="Underteckna!" id="sign" rel="#dialog-confirm-sign">
    <input class="bigbutton" type="submit" name="cancel" value="Avvisa" rel="#dialog-confirm-cancel" id="cancel">
  </div>
</div>
    
###
####
pageDocumentForAuthorHelpers=
<span class="templating_insert"> $signatoryEntry:noescape()$</span>,
<script type="text/javascript"> 
  var documentid = "$documentid$"; 
</script>
####
pageDocumentForAuthorHelpersLocalDialogs=
<div class="localdialogs">
  <form method="post" name="form" action="$linkissuedoc$" class="overlay redirectsubmitform" id="dialog-confirm-signinvite" rel=".stepForm">  
    <a class="close"> </a>
    <h2 id="dialog-title-sign">Underteckna</h2>
    <div id="dialog-confirm-text-sign">
      <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
      <p>När du undertecknat kommer en automatisk inbjudan att skickas till 
      <span class="Xinvited">Invited</span> med e-post.</p>
    </div>
    <div class="buttonbox" >
      <input type="hidden" name="final" value="automatic">
      <button class="close button" type="button"> Avbryt </button>
      <button class="submiter button" type="button"> Underteckna </button>
    </div>
  </form>

  <!--SJust sending a document is not supported yet. Kept it because someone put some work to texts -->
  <form method="post" name="form" action="$linkissuedoc$" class="overlay redirectsubmitform" id="dialog-confirm-sendinvite" rel=".stepForm">  
    <a class="close"> </a>
    <h2 id="dialog-title-send">Skicka inbjudan</h2>
    <div id="dialog-confirm-text-send">
      <p>Du har valt en sekreterarroll och kommer själv inte att underteckna. Är du säker på att du vill skicka en inbjudan att underteckna dokumentet <strong>$documenttitle$</strong> till <span class="Xinvited">Invited</span>?</p>
    </div>
    <div class="buttonbox" >
      <input type="hidden" name="final" value="automatic">
      <button class="close button" type="button"> Avbryt </button>
      <button class="submiter button" type="button"> Skicka </button>
    </div>
  </form>

  <form method="post" name="form" action=action id="dialog-confirm-sign-eleg" class="overlay">     
    <script type="text/javascript" src="/js/bankid.js"></script>
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
      <button class="editer button" type="button"> Skriv eget meddelande </button>
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
  <form method="post" name="form" action="$action$" id="dialog-confirm-sign-by-author" class="overlay">     
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
</div>

#####
####
pageDocumentForAuthorContent=
    $if(preparation)$
       $pageDocumentForAuthorContentPreparation()$
    $else$
       $pageDocumentForAuthorContentPostPreparation()$
    $endif$ 
#####
pageDocumentForAuthorContentPreparation=
<div style="display: none">
  $helpers:noescape()$
</div>
<div id="signStepsWrapper">

  <div id="signStepsContainer" class="follow">
    
    <div id="signStepsTitleRow">
      <span id="signStepsTitleRowTextContainer">
	<span class="title">DOKUMENT:</span>
	<span class="name">$documenttitle$ | </span> 
        <span class="title"> Transaktion #$documentid$ </span> 
	<!-- <span class="changeName"><a href="#changeName">BYT TITEL</a></span> -->
      </span>
   

  
      <div class="signStepsButton" id="signStepsNextButton">
	<a href="#nextStep" class="clickArea"></a>
	
	<a href="#nextStep"><span class="icon arrowRight"></span></a><!-- 005 no whitespace here pleace, or one pixel of the button will be unclicable pixel in IE --><a href="#nextStep">
	  <span class="text">Nästa steg</span>
	</a>
      </div>
     </div>
    
    <div id="signStepsMenu">
      <div id="step1select">
        <span class="step1select clickArea"></span>
        <span class="step1select step1icon"> </span>
	<span class="step1select text">Ladda upp dokument</span>
      </div>
      
      <div id="step2select" class="current">
	<a href="#" class="step2select clickArea"></a>
	<a href="#" class="step2select step2icon"> </a>
	<a href="#" class="step2select text">Fyll i uppgifter</a>
      </div>
      
      <div id="step3select">
	<a href="#" class="step3select clickArea"></a>
	<a href="#" class="step3select step3icon"> </a>
	<a href="#" class="step3select text">Underteckna och skicka!</a>
      </div>
      
      <div id="signStepsMenuEmptyStep"></div>
    </div>

    <div id="signStepsContentStack">

      <form action="" method="post" class="stepForm">
        <input type="hidden" name="allowedsignaturetypes"
               value="Email">

        <div id="signStep2Content">
          <div id="signStepsBody">
	    
            <div id="signStepsBodyLeft">
	      <div id="people">
	        <div class="signStepsBodyTitle">
	          <span>PERSONER</span>
	        </div>
	        
	        <div id="peopleList" class="signStepsBodyForms">
                  <ol>
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
	    
            <div id="personpane">
              <span class="persondetails authordetails">
                <div class="signStepsBodyTitle">
                  <span class="partnumber">PART 1</span>
                </div>
                
                <!-- 008 change/new -->
                <div class="signStepsBodyIcons">
                  <a href="#man2" class="man"></a>
                  <span class="briefcase"></span>
                  <span class="plus"></span>
                </div>
                
                <!-- 013 changed -->
                <div class="sigentry signStepsBodyForms" id="peopleDetails">
                  <span class="fields">
	            <span class='dragtext authorname'>
	              <span style="display:none;" class="fieldid">name</span>
	              <span style="display:none;" class="type">author</span>
	              <span class="fieldvalue"></span>
	              <img src="/theme/images/iconHand.png" class="draghandle"
	                   alt="drag" />
                    </span>
	            <span class='dragtext authorcomp'>
	              <span style="display:none;" class="fieldid">company</span>
	              <span style="display:none;" class="type">sig</span>
                   
                      <span class="fieldvalue"></span>
	              <img src="/theme/images/iconHand.png" class="draghandle"
	                   alt="drag" />
                    </span><br>
                    <span class='dragtext authoremail'>
	              <span style="display:none;" class="fieldid">email</span>
	              <span style="display:none;"
	              class="type">author</span>
                      <span class="fieldvalue"></span>
	              <img src="/theme/images/iconHand.png" class="draghandle"
	                   alt="drag" />
	            </span>
	            <span class='dragtext authornum'>
	              <span style="display:none;" class="fieldid">number</span>
	              <span style="display:none;"
	              class="type">sig</span>
                      <span class="fieldvalue"></span>
	              <img src="/theme/images/iconHand.png" class="draghandle" 
                           alt="drag" />
	            </span>
                  </span>
                  <div class="partyrole">
                    <a href="#" class="closelink"><small>close</small></a>
                    <input type="radio" name="authorrole" id="authorsignatoryradio" value="signatory" checked>Undertecknare</input>
                    <br>
                    <input type="radio" name="authorrole" id="authorsecretaryradio" value="secretary">Sekreterare</input>
                  </div>
                  
                </div>
                <div class="otherfields">
                  
                </div>
              </span>

            </div>
	    
            <div id="signStepsBodyFunctions">
	      <div id="signStepsBodyFunctionsBox">
	        <ul>
	          <!-- 008 changed/new -->
	          <li class="plus"><a href="#add"  id="addSignatory">LÄGG TILL PERSON</a></li>
	          <li class="minus"><a href="#remove"  id="delSignatory">TA BORT PERSON</a></li>
	          <li  class="floppyIcon submitStepFormOnClick"><a href="#save">SPARA SOM MALL</a></li> 
	          <li  class="arrowDown"><a href="$linkissuedocpdf$" target="_blank">LADDA NED SOM PDF</a></li>
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
                    <input type="checkbox" 
                           class="addremovecheckbox
                                  flashOnOn" 
                           rel="#daystosignbox"
                           location="#datetosigncontainer"
                           oldlocation="#hiddenttimestuffbox"
                           autocomplete="off"
                           value="$if(timetosignset)$on$else$off$endif$"
                           id="daystosigncheckbox" />
                    <span class="hidden flashMessage">Varning: Om du väljer ett förfallodatum kan du inte återkalla inbjudan innan datumet förfallit. Detta regleras av avtalslagen.</span>
                    <label for="daystosigncheckbox">Aktivera förfallodatum</label>
                    <div id="datetosigncontainer" style="padding-left:15px">  </div>
                 </span>
	        </p>
	        
	        <p>
	          <input type="checkbox" id="b">
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

	      <div id="signStepsBodyUploadBox" class="step3" style="display: none;">
                
	        <span id="signStepsBodyUploadTextContainer">
	          <span class="text">Bifoga dokument med inbjudan</span>
	        </span>
	        <br>
	        
	        <div id="signStepsButtonContainer">
	          <div class="signStepsButton">
		    <a href="#attachFile" class="clickArea"></a>
		    <a href="#attachFile"><span class="icon arrowUp"></span></a>
                    <a href="#attachFile"> <span class="text">Ladda upp fil</span> </a>
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
	        <input type="submit" id="signinvite" rel="#dialog-confirm-signinvite" value="" name="final"
	               class="buttonSignLarge" />
                <input type="submit" id="sendinvite"
	        rel="#dialog-confirm-sendinvite" value = ""
	        name="final" class="buttonSendLarge"
	        style="display:none;" />
	      </div>
	    </div>
	    
          </div>			
        </div>
      </form>
      $pageDocumentForAuthorHelpersLocalDialogs()$
    </div>
    <div id="signStepsBodyBorderBottom"></div>
  </div>
  
  <!-- span>signStepsBorderBottom går upp till titelRow om man tar bort denna text. förslag på lösning? Lösning: lägg till float:left på signStepsBodyBorderBottom /johan  </span -->
  
</div>		
</div>	
<div id="documentBox">
  <script type="text/javascript">
    var docstate = $docstate:noescape()$;
  </script>
  <script src="/js/templates.js" type="text/javascript"></script>
  <div class="waiting4page"></div>
</div>
<div id="templates" style="display:none;">
  <span class="customfield dragfield inputWrapper">
    <input type="text" value="" class="customfieldbox" autocomplete="off" />
    <a href="#minus2" class="minus"></a>
    <img src="/theme/images/iconHand.png" class="draghandle" alt="drag" />
  </span>
  <span class="persondetails">
    <div class="signStepsBodyTitle">
      <span class="partnumber">PART 1</span>
    </div>
    
    <!-- 008 change/new -->
    <div class="signStepsBodyIcons">
      <span href="#man2" class="man"></span>
      <span class="briefcase"></span>
      <a href="#plus2" class="plus"></a>
    </div>
    
    <!-- 013 changed -->
    <div class="sigentry signStepsBodyForms" id="peopleDetails">
      <input type="hidden" name="sigid" value="" />
      <span class="fields">
	<span class='dragfield signame inputWrapper'>
	  <span style="display:none;" class="fieldid">name</span>
	  <span style="display:none;" class="type">author</span>
	  <input type="text" infotext="För- & efternamn"
                 name="signatoryname" autocomplete="off" value="" />
	  <img src="/theme/images/iconHand.png" class="draghandle"
	       alt="drag" />
        </span>
	<span class='dragfield sigcomp inputWrapper'>
	  <span style="display:none;" class="fieldid">company</span>
	  <span style="display:none;" class="type">sig</span>
          <input type="text" infotext="Ev. Företagsnamn" name="signatorycompany" autocomplete="off">
	  <img src="/theme/images/iconHand.png" class="draghandle"
	       alt="drag" />
        </span><br>
        <span class='dragfield sigemail inputWrapper'>
	  <span style="display:none;" class="fieldid">email</span>
	  <span style="display:none;" class="type">author</span>
	  <input type="email" infotext="E-post" name="signatoryemail" autocomplete="off">
	  <img src="/theme/images/iconHand.png" class="draghandle"
	       alt="drag" />
	</span>
	<span class='dragfield signum inputWrapper'>
	  <span style="display:none;" class="fieldid">number</span>
	  <span style="display:none;" class="type">sig</span>
	  <input type="text" infotext="Org.-nr / Personnr." name="signatorynumber" autocomplete="off">
	  <img src="/theme/images/iconHand.png" class="draghandle" 
               alt="drag" />
	</span>
      </span>

    </div>
    <div class="otherfields">
      
    </div>
  </span>
</div>
####
pageDocumentForAuthorContentPostPreparation=
  <div class="localdialogs">
    <div style="display: none">
      $helpers:noescape()$
    </div>
  </div>
<div id="signStepsContainer"> 
  <div id="signStepsTitleRow"> 
     <span id="signStepsTitleRowTextContainer"> 
      <span class="title">DOKUMENT:</span>
      <span class="name">$documenttitle$ |</span>
      <span class="title"> Transaktion #$documentid$ </span> 
     </span>
     <div class="downloadButton" >
        <a href="$linkissuedocpdf$" target="_blank">
         <span class="icon arrowDownIcon"></span>
         <span class="text">Ladda ned PDF</span>
       </a>
   </div>
  </div>
  <div id="signStepsBody" class="forauthor"> 
    <div id="signViewBodyLeft" > 
      <span id="signViewBodyLeftTextContainer"> 
        $documentinfotext:noescape()$
      </span> 

  <div> 
   $if(awaitingauthor)$
      <BR/>
      <BR/>
      <input class="button" type="submit" name="sign" value="Underteckna!" id="signByAuthor" rel="#dialog-confirm-sign-by-author">
      <input class="button cancel bigbutton" type="button"
      name="cancel" value="Återkalla inbjudan"
      rel="#cancel-by-author-dialog">  
      <script type="text/javascript" src="/js/signatory.js" ></script>
   $$
   $elseif(pending)$
      <script type="text/javascript" src="/js/signatory.js" ></script>
      $if(!timetosignset)$
      <span>
        <BR/>
        <BR/>
        <input class="button cancel bigbutton" type="button" name="cancel" value="Återkalla inbjudan"  rel="#cancel-by-author-dialog">    
      </span>
    $else$

    <span>Du kan inte återkalla inbjudan före förfallodatum.</span>
   $endif$   
   $endif$      
   $if(canberestarted)$
   <BR/>
   <BR/>
   $restartForm:noescape()$
   $endif$ 
  </div>

    </div>
    $signatories:noescape()$

    <div id="signStepsBodyBorderBottom"></div>
  </div>
</div>

<div id="documentBox">
  <script type="text/javascript">
    var docstate = $docstate:noescape()$;
  </script>

  <!-- 
    I need to turn this stuff off since it brakes js now. TODO!!!!  
    <script type="text/javascript" src="/js/signatory.js"></script>
  -->
  <div class="waiting4page"></div>
</div>
$pageDocumentForAuthorHelpersLocalDialogs()$
###
documentInfoText= $if(pending)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Granska
                        </span><br>
                        <span class="text">
                          Vänligen var noga med att granska dokumentet och kontrollera uppgifterna nedan innan du undertecknar.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Väntar på motpart
                        </span><br>
                        <span class="text">
                            Du har undertecknat och vi inväntar övriga parter.
                        </span>
                     $else$
                         <span class="header">
                             Ej färdigställt
                        </span><br>
                        <span class="text">
                           Du har skickat inbjudan att underteckna. Dokumentet har ännu inte undertecknats av samtliga parter.
                        </span>
                     $endif$

                  $elseif(cancel)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Dokument återkallat
                        </span><br>
                        <span class="text">
                           $authorname$  har återkallat sin inbjudan att underteckna dokumentet.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Dokument återkallat
                        </span><br>
                        <span class="text">
                           $authorname$ har återkallat sin inbjudan att underteckna dokumentet.
                        </span>

                     $else$
                        <span class="header">
                             Dokument återkallat
                        </span><br>
                        <span class="text">
                           $authorname$ har återkallat sin inbjudan att underteckna dokumentet.
                        </span>
 
                     $endif$
 
                 $elseif(timedout)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Förfallodatum har passerat
                        </span><br>
                        <span class="text">
                           Du kan inte längre underteckna dokumentet eftersom sista datum för att underteckna passerat.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Förfallodatum har passerat
                        </span><br>
                        <span class="text">
                           Samtliga parter undertecknade inte dokumentet i tid. Förfallodatum har passerat och därför har dokumentet avbrutits.
                        </span>

                     $else$
                        <span class="header">
                             Förfallodatum har passerat
                        </span><br>
                        <span class="text">
                           Samtliga parter undertecknade inte dokumentet i tid. Förfallodatum har passerat och därför har dokumentet avbrutits.
                        </span>

                     $endif$

                  $elseif(rejected)$


                     $if(notsignedbyme)$    
                        <span class="header">
                             Avvisat
                        </span><br>
                        <span class="text">
                           Dokumentet har avvisats av en motpart.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Avvisat
                        </span><br>
                        <span class="text">
                           Dokumentet har avvisats av en motpart.
                        </span>

                     $else$
                        <span class="header">
                             Avvisat
                        </span><br>
                        <span class="text">
                           Dokumentet har avvisats av en motpart
                        </span>

                     $endif$

                  $elseif(signed)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Färdigställt
                        </span><br>
                        <span class="text">
                           Samtliga parter har undertecknat dokumentet.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Färdigställt
                        </span><br>
                        <span class="text">
                           Samtliga parter har undertecknat dokumentet.
                        </span>

                     $else$
                        <span class="header">
                             Färdigställt
                        </span><br>
                        <span class="text">
                           Samtliga parter har undertecknat dokumentet.
                        </span>

                     $endif$

                  $elseif(awaitingauthor)$

                     $if(notsignedbyme)$    
                        <span class="header">
                             Väntar
                        </span><br>
                        <span class="text">
                           Dokumentet väntar på att undertecknas av inbjudande part.
                        </span>

                     $elseif(signedbyme)$
                        <span class="header">
                             Väntar
                        </span><br>
                        <span class="text">
                           Dokumentet väntar på att undertecknas av inbjudande part.
                        </span>
                     $else$
                        <span class="header">
                             Väntar
                        </span><br>
                        <span class="text">
                           Dokumentet väntar på att undertecknas av inbjudande part.
                        </span>
                     $endif$


                  $endif$
######                  
                   