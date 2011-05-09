####
signatoriesBox=  
        <div class="float-left signViewBodyBox">$signatories:{signatory| $if(signatory.current)$$signatoryBox()$$endif$}$</div>
        <div class="float-right signViewBodyBox"> $signatories:{signatory| $if(!signatory.current)$$signatoryBox()$$endif$}$ </div>
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
    $if(contract)$  
        Undertecknat $signatory.signdate$
    $elseif(offer)$
       Accepterat offert $signatory.signdate$
    $endif$
$elseif(signatory.datamismatch)$
        Felaktig e-legitmation
$elseif(timedout)$
        Förfallodatum har passerat
$elseif(cancel)$
     $if(contract)$  
     $elseif(offer)$
          Offert återkallad
     $endif$
$elseif(datamismatch)$

$elseif(signatory.rejecteddate)$
    Avvisat $signatory.rejecteddate$
$elseif(signatory.seendate)$        
    Granskat online $signatory.seendate$
$elseif(signatory.readdate)$
    Läst e-post $signatory.readdate$
$else$
    $if(contract)$  
        Har ej granskat online
    $elseif(offer)$
        Har ej granskat online
    $endif$
$endif$
###       
secretaryMessage = 
    $if(contract)$  
       Avsändare: Ej undertecknande part
    $elseif(offer)$
       Avsändare
    $endif$
### FORM and button for reminding message
###
reminderForm = 
<span>
  <a  class=" btn-tiny green prepareToSendReminderMail" rel="#siglnk$signatorylinkid$">  
    <div class="$if(signatory.signdate)$ reminderForSignedIcon $else$ reminderForSendIcon $endif$"></div>
    <div class="sendLinkText">
        $if(signatory.signdate)$
            $if(contract)$
                Skicka dokumentet igen 
            $elseif(offer)$
                Skicka offert igen
            $endif$
        $else$ 
            Skicka påminnelse 
        $endif$
    </div> 
  </a>
  <form class="overlay" action="$signatory.linkremind$" method="POST" id="siglnk$signatorylinkid$">
    <div class="modal-container">
        <div class="modal-header">
            <span class="modal-icon message"></span>
            <span class="modal-title">
                  <h2> 
                    $if(signatory.signdate)$
                        $if(contract)$
                            Skicka dokumentet igen 
                        $elseif(offer)$
                            Skicka offert igen
                        $endif$
                    $else$ 
                        Skicka påminnelse 
                    $endif$
                  </h2>
            </span>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content">
                 $signatory.reminderMessage:noescape()$
            </div>
        </div>
        <div class="modal-footer">
           <input type="hidden" name="sign" value="automatic" />
           <div class="float-left">
                <a  class="close cancel">Avbryt</a>
                <a  class="editer message"> Skriv eget meddelande </a>
           </div>
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
