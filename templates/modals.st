###
modalSignInviteView=
    <div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Du har undertecknat dokumentet <strong>$documenttitle$</strong>!</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>En inbjudan att underteckna har nu skickats 
            till $partyListButAuthor:noescape()$.
          </p>
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Stäng</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>

######
modalWelcomeToSkrivaPa=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>2 månader gratis!</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
        <p>Som ett erbjudande bjuder vi på 2 månaders fri användning av tjänsten SkrivaPå. Med SkrivaPå kan du underteckna och arkivera avtal och andra dokument elektroniskt. Givetvis kommer det inte att kosta någonting efter de 2 fria månaderna om du väljer att inte fortsätta. Det är superenkelt att komma igång, gör så här:<p>

        <p>1. Ladda upp ett dokument<br>
        2. Fyll i motpartens uppgifter<br>
        3. Underteckna och skicka iväg</p>

        <p>Testa på dig själv först, det är faktiskt roligt!</p>

        <p>Med vänliga hälsningar<br>
        Team SkrivaPå</p>
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Stäng fönster</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
</div>
####
modalAccountRemoval=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Är du säker?</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>Är du säker på att du vill att vi raderar dokumentet "$documenttitle$" och ditt konto från SkrivaPå?<p>
        </div>
      </div>
      <div class="modal-footer">
        <a href="$activationlink$" class="btn-small green float-right">
          <div class="left"></div>
          <div class="label">
            <p>Aktivera konto</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
        <form method="post" action="$removallink$" name="removeaccountform"></form>
        <a href="#" class="float-left cancel" onclick="document.removeaccountform.submit();return false;">Ja</a>
      </div>
</div>
###
modalAccountRemoved=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Raderat</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>Din kopia av dokumentet "$documenttitle$" är nu raderat tillsammans från SkrivaPå tillsammans med ditt konto.</p>
          <p>Med vänliga hälsningar<br>
          Team SkrivaPå</p>
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Ok</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
</div>
###
modalSendInviteView=
    <div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Dokumentet <strong>$documenttitle$</strong> skickat!</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>En inbjudan att underteckna har nu skickats
            till $partyListButAuthor:noescape()$.
          </p>
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Stäng</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>
######
######
modalLoginForSaveView=
    <div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Konto har skapats</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
           <p>Ditt konto har skapats. En aktiveringslänk har skickats till din inkorg.</p>
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Stäng</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>
####
#####
modalRejectedView=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Du har avvisat dokumentet $documenttitle$</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>Ett meddelande har skickats till $partyList:noescape()$.</p>
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Stäng</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
</div>
#####
####
modalSignedViewClosed=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Du har undertecknat dokumentet $documenttitle$</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
         <p>$partyListString:noescape()$ har nu undertecknat och dokumentet är bindande.</p>
         $willCreateAccountForYou:noescape()$
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Stäng</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
</div>
#####
modalSignedViewNotClosed=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Du har undertecknat dokumentet $documenttitle$</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
         <p>$partyUnsignedListString:noescape()$ har ännu inte undertecknat. När alla undertecknat blir
         dokumentet bindande och en kopia skickas till din e-post.</p>
         $willCreateAccountForYou:noescape()$
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Stäng</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
</div>
#####
willCreateAccountForYouHasAccount=
    <p>Dokumentet har sparats till ditt konto.
       $if(!isloggedin)$
       $loginModalPart()$
       $endif$
    </p>
#####
willCreateAccountForYouNoAccount=
<p>
  Av erfarenhet från många kundsamtal vet vi att även de viktigaste dokument kan komma bort. Viktiga papper bör inte få komma bort. Därför, för din juridiska säkerhet har vi skapat ett konto åt dig på SkrivaPå där vi sparat dokumentet "$documenttitle$". Du måste dock definiera ditt lösenord inom 24h. Vi har skickat ett separat mail med en kort instruktion.
</p>
####
loginModal=
$if(!logged)$
$if(loginModal)$
<div class="modalcontainer">
 <div class="modalbox overlay login-container">
  <div class="modal-container">
   <form method="post" action="$linklogin$">
    <div class="modal-header">
     <div class="modal-icon message"></div>
     <div class="modal-title">
      <h1>Logga in</h1>
     </div>
     <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
     <div class="modal-content"> 
      <div class="body">
       <div class="wrapper" style="text-align:right">
           <input type="hidden" name="referer" value="$referer$" />
           <span class="txt">E-post</span><input type="email" name="email" value="$email$" infotext="E-post" autocomplete="off"/><br />
           <span class="txt">Lösenord</span><input type="password" name="password" infotext="password" autocomplete="off"/><br />
           <input type="submit" class="hidden" />
       </div>
      </div>
      </div>
     </div>
     <div class="modal-footer">
        <a class="close cancel">Avbryt</a>
        <a href="#" class="txt-link message">Glömt lösenord</a>
        <a href="#" class="btn-small blue float-right submit">
          <div class="left"></div>
          <div class="label">
            <p>Logga in</p>
          </div>
          <div class="right"></div>
        </a>
     </div>
     </form>
  </div>
 </div>
 
 <div class="modalbox overlay recovery-container">
  <div class="modal-container">
   <form method="post" action="$linkforgotenpassword$">
    <div class="modal-header">
      <div class="modal-icon message"></div>
      <div class="modal-title">
          <h1>Glömt lösenord</h1>
      </div>
      <a class="modal-close close"> </a>
     </div>
    <div class="modal-body">
      <div class="modal-content"> 
       <div class="body">
         <div class="wrapper">
               <input type="email" name="email" infotext="E-post" autocomplete="off"/>
               <input type="submit" class="hidden" />
         </div>
       </div>
      </div>
    </div>  
    <div class="modal-footer">
        <a class="close cancel">Avbryt</a>
        <a href="#" class="txt-link message">Logga in</a>
        <a href="#" class="btn-small green float-right submit">
          <div class="left"></div>
          <div class="label">
            <p>Skicka nytt lösenord</p>
          </div>
          <div class="right"></div>
        </a>
      </div>
   </form>
  </div>
 </div>
</div>
$endif$
$endif$
####
loginModalPart=
<div class="user-container">
                                                <div class="login-container">
                                                        <div class="body">
                                                           <div class="wrapper">
                                                                <form method="post" action="$linklogin$">
                                                                        <input type="hidden" name="referer" value="$referer$" />
                                                                        <input type="email" name="email" value="$email$" infotext="E-post" autocomplete="off"/><br />
                                                                        <input type="password" name="password" infotext="password" autocomplete="off"/><br />
                                                                            <a href="#" class="btn-tiny blue center submit">
                                                                            <div class="left"> </div>     
                                                                            <div class="label"><span>Logga in</span> </div>
                                                                            <div class="right"/> </div>
                                                                        </a>
                                                                        <br /><br />
                                                                        <input type="submit" class="hidden" />
                                                                </form>
                                                           </div>
                                                        </div>
                                                </div>
                                              
</div>
######
####
modalNewPasswordView=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h1>Nytt Lösenord</h1>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content"> 
                $newPasswordModalPart()$
        </div>
      </div>
</div>
####
newPasswordModalPart=
<div class="user-container">
  <div class="login-container">
    <div class="body">
      <form action="$linkchangepassword$" method="post">
        <div class="txt">Nytt lösenord:</div>
        <input type="password" name="password" autocomplete="off" />
        <div class="txt">Upprepa lösenord:</div>
        <input type="password" name="password2" autocomplete="off" />
        <a href="#" class="btn-tiny blue submit">
          <div class="left"> </div>     
          <div class="label"><span>Spara</span></div>
          <div class="right"/> </div>
        </a>
        <input type="submit" class="hidden"/>
      </form>
    </div>
  </div>
</div>
######
###
modalError=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Oops, ett fel har uppstått</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>Oroa dig inte, vi tittar på problemet</p>
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Ok</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
</div>
###
signCanceledDataMismatchModal=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Felaktiga personuppgifter</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
             <p>Vi beklagar. Uppgifterna i din e-legitimation matchar inte dina personuppgifter.</p>
             <p>Avtalsprocessen är därför bruten, var vänliga invänta <br> 
                en ny inbjudan eller kontakta <a href="mailto:$authoremail$">$authorname$</a>.</p>
             <p><strong>Felrapport:</strong></p>
             $message:noescape()$
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Ok</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
</div>
###
signAwaitingAuthorLast=
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h3>Dokumentet har undertecknats</h3>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
             <p>Alla parter har nu undertecknat och dokumentet är bindande.</p>
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Ok</p>
            <div class="btn-symbol green arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
</div>
###

