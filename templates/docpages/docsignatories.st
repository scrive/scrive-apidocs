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
            $fieldvalue(value=field.fieldvalue)$
        </div>}$
     $endif$
   </span>
   <span class="signViewBodyForms"></span>
   $if(!addSignatoryScript)$
   $if(signatory.company)$
    <div class="field">
        $fieldvalue(value=signatory.company)$
    </div>
   $endif$
   $if(signatory.personalnumber)$
     <div class="field">
     $fieldvalue(value=signatory.personalnumber)$
     </div>
   $endif$
   $if(signatory.companynumber)$
     <div class="field">
      $fieldvalue(value=signatory.companynumber)$
     </div>
   $endif$
   $endif$
   $if(signatory.email)$
     <div class="field">
           $fieldvalue(value=signatory.email)$
           <span class="fieldvalue" style="display:none">$signatory.email$</span>
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
fieldvalue=
<input class="fieldvalue" type="text" value="$value$" readonly=""
       autocomplete="off"> </input>
###
