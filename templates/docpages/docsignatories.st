####
signatoriesBox=  
<div class="float-left signViewBodyBox">$signatories:{signatory|
  $if(signatory.current)$ $signatoryBox()$ $endif$ }$ </div>
<div class="float-right signViewBodyBox">$signatories:{signatory|
  $if(!signatory.current)$ $signatoryBox()$ $endif$ }$ </div>
####
signatorysignedcontracttext=$_signatorysignedcontracttext()$
####
signatorysignedoffertext=$_signatorysignedoffertext()$
####
signatorysignedordertext=$_signatorysignedordertext()$
####
signatorycanceledcontracttext=$_signatorycanceledcontracttext()$
####
signatorycanceledoffertext=$_signatorycanceledoffertext()$
####
signatorycanceledordertext=$_signatorycanceledordertext()$
####
contractauthorissecretarytext=$_contractauthorissecretarytext()$
####
offerauthorissecretarytext=$_offerauthorissecretarytext()$
####
orderauthorissecretarytext=$_orderauthorissecretarytext()$
####
contractremindagainbuttontext=$_contractremindagainbuttontext()$
####
offerremindagainbuttontext=$_offerremindagainbuttontext()$
####
orderremindagainbuttontext=$_orderremindagainbuttontext()$
####
signatoryBox =
<div class="signViewBodyRight  $if(signatory.author)$author$endif$">
  <div class="signViewBodyRightTextContainer"> 
  <div class="header">$signatory.fstname$ $signatory.sndname$</div>
   <span class="signatoryfields">
     $if(!addSignatoryScript)$
     $signatory.fields:{field|
        <div class="field" name="field.fieldlabel">
            <span class="fieldvalue">$field.fieldvalue$</span>
        </div>}$
     $endif$
   </span>
   <span class="signViewBodyForms"></span>
   $if(!addSignatoryScript)$
   $if(signatory.company)$
    <div class="field">
        <span class="fieldvalue">$signatory.company$</span>
    </div>
   $endif$
   $if(signatory.personalnumber)$
     <div class="field">  
      <span class="fieldvalue">$signatory.personalnumber$</span>
     </div>
   $endif$
   $if(signatory.companynumber)$
     <div class="field">  
      <span class="fieldvalue">$signatory.companynumber$</span>
     </div>
   $endif$
   $endif$
   $if(signatory.email)$
     <div class="field">
         <span class="fieldvalue">$signatory.email$</span>
     </div>
   $endif$
   $if(!signatory.secretary)$ 
   <div class="text">
    <div class="icon status $signatory.status$"></div> 
    $if(signatory.undeliveredEmail)$ <span style="color:#000000;position:relative;left:-4px;">!</span>  $endif$
    <span class="textstatus">$signatoryMessage()$</span>
   </div>
   $if(signatory.allowRemindForm)$ $reminderForm()$ $endif$
   $if(signatory.allowEmailChange)$ $changeEmailForm()$ $endif$   
  $else$
    <div class="text"> $secretaryMessage()$ </div>
  $endif$
  </div>
</div>
####
#### Message that is put next to signatory name in document view
####
signatoryMessage=
$if(signatory.signdate)$
    $process.signatorysignedtext$ $signatory.signdate$
$elseif(signatory.datamismatch)$
        $_signatoryMessageIfSignatoryDatamismatch()$
$elseif(timedout)$
        $_signatoryMessageIfTimedout()$
$elseif(cancel)$
     $process.signatorycanceledtext$
$elseif(datamismatch)$

$elseif(signatory.rejecteddate)$
    $_signatoryMessageIfSignatoryRejecteddate()$	$signatory.rejecteddate$
$elseif(signatory.seendate)$        
    $_signatoryMessageIfSignatorySeendate()$ $signatory.seendate$
$elseif(signatory.readdate)$
    $_signatoryMessageIfSignatoryReaddate()$ $signatory.readdate$
$elseif(signatory.deliveredEmail)$
    $_signatoryMessageIfSignatoryDeliveredEmail()$
$else$
    $_signatoryMessageIfElse()$
$endif$
###       
secretaryMessage = 
    $process.authorissecretarytext$
### FORM and button for reminding message
###
reminderForm = 
<span>
  <a  class=" btn-tiny green prepareToSendReminderMail" rel="#siglnk$signatory.id$">  
    <div class="$if(signatory.signdate)$ reminderForSignedIcon $else$ reminderForSendIcon $endif$"></div>
    <div class="sendLinkText">
        $if(signatory.signdate)$
            $process.remindagainbuttontext$
        $else$ 
            $_sendReminder()$
        $endif$
    </div> 
  </a>
  <form class="overlay" action="$signatory.linkremind$" method="POST" id="siglnk$signatory.id$">
    <div class="modal-container">
        <div class="modal-header">
            <span class="modal-icon message"></span>
            <span class="modal-title">
                  <h2> 
                    $if(signatory.signdate)$
                        $process.remindagainbuttontext$
                    $else$ 
                        $_reminderFormHead()$
                    $endif$
                  </h2>
            </span>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content disablelinks">
                 $signatory.reminderMessage:noescape()$
            </div>
        </div>
        <div class="modal-footer">
           <input type="hidden" name="sign" value="automatic" />
           <div class="float-left">
                <a  class="close cancel">$_cancel()$</a>
                <a  class="editer message">$_reminderFormFooterOwnMessage()$</a>
           </div>
           <a href="#" class="btn-small green float-right submiter">
                <div class="left"></div>
                <div class="label">
                   $if(signatory.signdate)$ $_send()$ $else$ $_reminderFormSendReminder()$$endif$
                <div class="btn-symbol green arrow-left"></div></div>
                <div class="right"></div>
            </a>
        </div>
    </div>
  </form>     
</span>       
####
#### For change signatory email
####
changeEmailForm =  
<div style="margin-top: 10px">
<a class="btn-tiny blue replacebynextonclick">
                <div class="left"></div>
                <div class="label">
                     $_send()$
                </div>
                <div class="right"></div>
  </a>
<form action="$signatory.linkchangeemail$" method="POST" style="display:none">
  <input type="text" class="shiny" style="width:140px" name="email" value="$signatory.email$" />
  <a class="btn-tiny blue submit">
                <div class="left"></div>
                <div class="label">
                     $_send()$
                </div>
                <div class="right"></div>
  </a>
</form>     
</div>
####
