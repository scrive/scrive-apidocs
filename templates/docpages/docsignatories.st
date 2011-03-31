####
signatoriesBox=  
   <div class="signatoriesBox"> 
   <div class="currentSignatoryBox">
        $signatories:{signatory| $if(signatory.current)$$signatoryBox()$$endif$}$
   </div>
   <div class="otherSignatoriesBox">
        $signatories:{signatory| $if(!signatory.current)$$signatoryBox()$$endif$}$
   </div> 
   </div>
####
signatoryBox =
<div class="signViewBodyRight">
  <div class="signatoryLockIcon icon $if(signatory.current)$ lock-opened $else$ lock-closed $endif$"></div>
  <div class="signViewBodyRightTextContainer"> 
   <div class="header">$signatory.fstname$ $signatory.sndname$</div>
   <div class="text">
    <div class="icon status $signatory.status$"></div> 
    $if(signatory.undeliveredEmail)$ <span style="color:#000000;position:relative;left:-4px;">!</span>  $endif$
    $signatoryMessage()$
   </div>
   <span class="signatoryfields">
     $if(!addSignatoryScript)$
     $signatory.fields:{field|
        <div class="field" name="field.fieldlabel">
            <div class="icon lock-closed"></div>
            <span class="fieldvalue">$field.fieldvalue$</span>
        </div>}$
     $endif$
   </span>
   <span class="signViewBodyForms"></span>
   $if(!addSignatoryScript)$
   $if(signatory.company)$
    <div class="field">
        <div class="icon lock-closed"></div>
        <span class="fieldvalue">$signatory.company$</span>
    </div>
   $endif$
   $if(signatory.number)$
     <div class="field">  
      <div class="icon lock-closed"></div>
      <span class="fieldvalue">$signatory.number$</span>
     </div>
   $endif$
   $endif$
   $if(signatory.email)$
     <div class="field">
         <div class="icon lock-closed"></div>
         <span class="fieldvalue">$signatory.email$</span>
     </div>
   $endif$
   $if(signatory.allowRemindForm)$ $reminderForm()$ $endif$
   $if(signatory.allowEmailChange)$ $changeEmailForm()$ $endif$   
  </div>
</div>
####
#### Message that is put next to signatory name in document view
####
signatoryMessage=
$if(signatory.signdate)$
        Undertecknat $signatory.signdate$
$elseif(timedout)$
        Förfallodatum har passerat
$elseif(cancel)$

$elseif(signatory.seendate)$        
Granskat $signatory.seendate$
$else$
Har ej undertecknat
$endif$
               
###       
### FORM and button for reminding message
###
reminderForm = 
<span>
  <a  class=" btn-tiny green prepareToSendReminderMail" rel="#siglnk$signatorylinkid$">  
    <div class="$if(signatory.signdate)$ reminderForSignedIcon $else$ reminderForSendIcon $endif$"></div>
    <div class="sendLinkText">$if(signatory.signdate)$ Skicka dokumentet igen $else$ Skicka påminnelse $endif$</div> 
  </a>
  <form class="overlay" action="$signatory.linkremind$" method="POST" id="siglnk$signatorylinkid$">
    <div class="modal-container">
        <div class="modal-header">
            <div class="modal-icon message"></div>
            <div class="modal-title">
                  <h2> $if(signatory.signdate)$ Skicka dokumentet igen $else$ Skicka påminnelse $endif$ </h2>
            </div>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content">
                 $signatory.reminderMessage:noescape()$
            </div>
        </div>
        <div class="modal-footer">
               <input type="hidden" name="sign" value="automatic" />
                <a  class="close cancel">Avbryt</a>
                <a  class="editer message"> Skriv eget meddelande </a>
                <a href="#" class="btn-small green float-right submiter">
                <div class="left"></div>
                <div class="label">
                   $if(signatory.signdate)$ Skicka $else$ Skicka påminnelse $endif$
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
<div>
<a class="btn-tiny blue replacebynextonclick">
                <div class="left"></div>
                <div class="label">
                     Skicka inbjudan till ny adress
                </div>
                <div class="right"></div>
  </a>
<form action="$signatory.linkchangeemail$" method="POST" style="display:none">
  <input type="text" class="shiny" style="width:140px" name="email" value="$signatory.email$" />
  <a class="btn-tiny blue submit">
                <div class="left"></div>
                <div class="label">
                     Skicka
                </div>
                <div class="right"></div>
  </a>
</form>     
</div>
####