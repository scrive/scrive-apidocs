###
modalSignInviteView=
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Du har undertecknat dokumentet <strong>$documenttitle$</strong>!</h3>
        </span>
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
###
modalOfferCreated=
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Offerten skickad!</h3>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p> Offerten <strong>$documenttitle$</strong> har nu skickats till <strong>$signatory$</strong>. 
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
modalOfferSigned=
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Du har accepterat offerten!</h3>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
            <p> Offerten har sparats till ditt konto med användarnamn <span style="color:#007CAC">$signatory$</span>.
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Vi bjuder på 1 fri månad med SkrivaPå!</h3>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
        <p>Med SkrivaPå kan du skriva på och arkivera dokument elektroniskt. Tjänsten går att använda för avtal, offerter och även andra typer av dokument. Givetvis kommer det inte att kosta någonting efter den fria månaden om du väljer att inte fortsätta. Det är superenkelt att komma igång, gör så här:<p>

        <p>1. Ladda upp ett dokument<br>
        2. Fyll i motpartens uppgifter<br>
        3. Underteckna och skicka iväg</p>

        <p>Tips: Testa med dig själv först.</p>

        <p>Med vänliga hälsningar<br>
        Team SkrivaPå</p>
        </div>
      </div>
      <div class="modal-footer">
        <a class="btn-small green float-right close">
          <div class="left"></div>
          <div class="label">
            <p>Välkommen!</p>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Är du säker?</h3>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          <p>Är du säker på att du vill radera dokumentet "Non Disclosure and Immaterial Rights Agreement" och ditt konto från SkrivaPå?<p>
        </div>
      </div>
      <div class="modal-footer">
        <form method="post" action="$removallink$" id="removeaccountform"></form>
        <a href="#" onclick="\$('#removeaccountform').submit();" class="btn-small red float-right">
          <div class="left"></div>
          <div class="label">
            <p>Radera konto</p>
            <div class="btn-symbol red arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
        <a href="$activationlink$" class="float-left" style="color: #5eb30c; text-decoration: none">Aktivera konto</a>
      </div>
</div>
###
modalAccountRemoved=
<div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Raderat</h3>
        </span>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Dokumentet <strong>$documenttitle$</strong> skickat!</h3>
        </span>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Konto har skapats</h3>
        </span>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Du har avvisat dokumentet $documenttitle$</h3>
        </span>
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
      <span class="modal-icon message"></span>
      <span class="modal-title">
        <h3>Du har undertecknat dokumentet $documenttitle$</h3>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <p>$partyListString:noescape()$ har nu undertecknat och dokumentet är bindande. Ett exemplar av det färdigställda dokumentet har skickats till din inkorg.</p>
        $willCreateAccountForYou()$
      </div>
    </div>
    <div class="modal-footer">
      $if(hasaccount)$
      $if(!isloggedin)$
      <a class="float-left close text-link message">
          Stäng
      </a>
      <a href="#" class="btn-small blue float-right login-button notForIE7">
        <div class="left"></div>
        <div class="label">
          <p>Logga in</p>
        </div>
        <div class="right"></div>
      </a>
      $else$
      <a class="btn-small green float-right close">
        <div class="left"></div>
        <div class="label">
          <p>Stäng</p>
          <div class="btn-symbol green arrow-left"></div>
        </div>
        <div class="right"></div>
      </a>
      $endif$
      $else$
      <a class="btn-small green float-right close">
        <div class="left"></div>
        <div class="label">
          <p>Stäng</p>
          <div class="btn-symbol green arrow-left"></div>
        </div>
        <div class="right"></div>
      </a>
      $endif$
    </div>
</div>
#####
modalSignedViewNotClosed=
<div class="modal-container">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
        <h3>Du har undertecknat dokumentet $documenttitle$</h3>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <p>$partyUnsignedListString:noescape()$ har ännu inte undertecknat. När alla undertecknat blir
          dokumentet bindande och en kopia skickas till din e-post.</p>
        $willCreateAccountForYou()$
      </div>
    </div>
    <div class="modal-footer">
      $if(hasaccount)$
      <a class="float-left close text-link message">
          Stäng
      </a>
      <a href="#" class="btn-small blue float-right login-button notForIE7">
        <div class="left"></div>
        <div class="label">
          <p>Logga in</p>
        </div>
        <div class="right"></div>
      </a>
      $else$
      <a class="btn-small green float-right close">
        <div class="left"></div>
        <div class="label">
          <p>Stäng</p>
          <div class="btn-symbol green arrow-left"></div>
        </div>
        <div class="right"></div>
      </a>
      $endif$
    </div>
</div>
#####
willCreateAccountForYou=
        $if(hasaccount)$
        $willCreateAccountForYouHasAccount()$
        $endif$
#####
willCreateAccountForYouHasAccount=
  <p>Dokumentet har också sparats till ditt konto med användarnamn <span style="color:#007CAC">$email$</span></p>
#####
loginModal=
$if(!logged)$
$if(loginModal)$
<div class="modalcontainer">
 <div class="modalbox overlay login-container">
  <div class="modal-container">
   <form method="post" action="$linklogin$">
    <div class="modal-header">
     <span class="modal-icon message"></span>
     <span class="modal-title">
      <h1>Logga in</h1>
     </span>
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
        <div class="float-left">
          <a class="close cancel">Avbryt</a>
          <a href="#" class="txt-link message">Glömt lösenord</a>
        </div>
        <a href="#" class="btn-small blue float-right submit notForIE7">
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
      <span class="modal-icon message"></span>
      <span class="modal-title">
          <h1>Glömt lösenord</h1>
      </span>
      <a class="modal-close close"> </a>
     </div>
    <div class="modal-body">
      <div class="modal-content"> 
       <div class="body">
         <div class="wrapper">
           <span class="txt">E-post</span><input type="email" name="email" infotext="E-post" autocomplete="off"/>
           <input type="submit" class="hidden" />
         </div>
       </div>
      </div>
    </div>  
    <div class="modal-footer">
        <div class="float-left">
          <a class="close cancel">Avbryt</a>
          <a href="#" class="txt-link message">Logga in</a>
        </div>
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
      <div class="wrapper" style="text-align:right">
        <input type="hidden" name="referer" value="$referer$" />
        <span class="txt">E-post</span><input type="email" name="email" value="$email$" infotext="E-post" autocomplete="off"/><br />
        <span class="txt">Lösenord</span><input type="password" name="password" infotext="password" autocomplete="off"/><br />
        <input type="submit" class="hidden" />
      </div>
    </div>
  </div>
</div>
######
####
modalNewPasswordView=
<div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h1>Nytt Lösenord</h1>
        </span>
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
     <div class="wrapper">
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
</div>
######
###
modalError=
<div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Oops, ett fel har uppstått</h3>
        </span>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Felaktiga personuppgifter</h3>
        </span>
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
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h3>Dokumentet har undertecknats</h3>
        </span>
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

