###
modalSignInviteView=
<div class="modal-container">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    $if(signed)$
    <span class="modal-title">
      <h3>Du har undertecknat dokumentet <strong>$documenttitle$</strong>!</h3>
    </span>
    $else$
    <span class="modal-title">
      <h3>Du har undertecknat!</h3>
    </span>
    $endif$
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $if(signed)$
      <p>
        Du har undertecknat dokumentet $documenttitle$ och dokumentet har färdigställts.
      </p>
      $else$
      <p>
        En inbjudan att underteckna har nu skickats till
        $partyListButAuthor:noescape()$.
      </p>
      $endif$
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
####
modalOfferSignedHasAccount=
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
####
modalOfferSignedNoAccount=
<div class="modal-container">
  <form id="accountFromSignForm" method="post" action="$linkaccountfromsign$">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>Du har accepterat offerten!</h3>
      <p>
        Offerten är nu bindande. SkrivaPå har verifierat accepten och utfärdat ett verifikat. Ett original av den färdiga accepten har skickats till din inkorg.
      </p>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $accountFromSignInputs()$
    </div>
  </div>
  <div class="modal-footer">
    $accountFromSignFooter()$
  </div>
  </form>
</div>
######
modalAccountSetup=
<div class="modal-container">
  <script src="/js/signup.js" type="text/javascript"></script>
  <form action="$signuplink$" method="post">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>Vänligen acceptera användarvillkoren</h3>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body" style="padding: 10px">
    <div class="modal-content">
      <div style="max-height: 300px; overflow: auto">
        $termsPageContents()$
      </div>
      <div style="padding: 0 19px">
        <input type="checkbox" id="tosCBox" name="tos" class="notForIE7" />
        <label for="tosCBox">Jag har läst och accepterar SkrivaPå Allmänna Villkor</label><br/>
        <table id="accountTypeChoice" style="width: 250px; margin-bottom: 5px"><tbody><tr>
            <td><input id="accountTypePrivate" type="radio" name="accounttype" value="private" /> <label for="accountTypePrivate">Privatperson</label></td>
            <td><input id="accountTypeCompany" type="radio" name="accounttype" value="company" /> <label for="accountTypeCompany">Företag</label></td>
        </tr></tbody></table>
        <table id="companyInfo"><tbody>
            <tr>
                <td style="width: 120px">Förnamn:</td>
                <td><input type="text" name="fname" autocomplete="off" value="$if(fstname)$$fstname$$endif$"></td>
            </tr>
            <tr>
                <td>Efternamn:</td>
                <td><input type="text" name="lname" autocomplete="off" value="$if(sndname)$$sndname$$endif$"></td>
            </tr>
            <tr>
                <td>Företag:</td>
                <td><input type="text" name="companyname" autocomplete="off" value="$if(companyname)$$companyname$$endif$"></td>
            </tr>
            <tr>
                <td>Position:</td>
                <td><input type="text" name="companyposition" autocomplete="off" value="$if(companyposition)$$companyposition$$endif$"></td>
            </tr>
            <tr>
                <td>Telefonnummer:</td>
                <td><input type="text" name="phone" autocomplete="off" value="$if(phone)$$phone$$endif$"></td>
            </tr>
        </tbody></table>
        <table id="passwordInfo"><tbody>
            <tr>
                <td style="width: 120px">Välj lösenord:</td>
                <td><input type="password" name="password" autocomplete="off"></td>
            </tr>
            <tr>
                <td>Upprepa lösenord:</td>
                <td><input type="password" name="password2" autocomplete="off"></td>
            </tr>
        </tbody></table>
        <input type="submit" class="hidden" />
      </div>
    </div>
  </div>
  <div class="modal-footer">
    <a href="#" class="btn-small green float-right submit notForIE7">
      <div class="left"></div>
      <div class="label">
        <p>Skapa konto!</p>
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
modalSignedViewClosedHasAccount=
$if(isloggedin)$
$modalSignedViewClosedHasAccountLoggedIn()$
$else$
$modalSignedViewClosedHasAccountNotLoggedIn()$
$endif$
####
modalSignedViewClosedHasAccountNotLoggedIn=
<div class="modal-container">
  <form method="post" action="$linklogin$">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
        <h3>Du har undertecknat dokumentet $documenttitle$</h3>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <p>
          $partyListString:noescape()$ har nu undertecknat och
          dokumentet är bindande.
        </p>
        <p>
          Dokumentet har sparats till ditt konto.
        </p>
        $loginModalPart()$
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
        <a class="close cancel">Stäng</a>
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
####
modalSignedViewClosedHasAccountLoggedIn=
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
      <p>
        $partyListString:noescape()$ har nu undertecknat och
        dokumentet är bindande.
      </p>
      <p>
        Dokumentet har sparats till ditt konto.
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
####
modalSignedViewClosedNoAccount=
<div class="modal-container">
  <form id="accountFromSignForm" method="post" action="$linkaccountfromsign$">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>Du har undertecknat!</h3>
      <p>
        Dokumentet är nu bindande. SkrivaPå har verifierat denna transaktion och utfärdat ett verifikat. Vi har skickat ett original av dokumentet till din inkorg.
      </p>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $accountFromSignInputs()$
    </div>
  </div>
  <div class="modal-footer">
      $accountFromSignFooter()$
  </div>
</div>
####
modalSignedViewNotClosedHasAccount=
$if(isloggedin)$
$modalSignedViewNotClosedHasAccountLoggedIn()$
$else$
$modalSignedViewNotClosedHasAccountNotLoggedIn()$
$endif$
#####
modalSignedViewNotClosedHasAccountLoggedIn=
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
      <p>
        $partyUnsignedListString:noescape()$ har ännu inte
        undertecknat. När alla undertecknat blir dokumentet bindande
        och en kopia skickas till din e-post.
      </p>
      <p>
        Dokumentet har sparats till ditt konto.
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
#####
#####
modalSignedViewNotClosedHasAccountNotLoggedIn=
<div class="modal-container">
  <form method="post" action="$linklogin$">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
        <h3>Du har undertecknat dokumentet $documenttitle$</h3>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">
      <div class="modal-content">
        <p>
          $partyUnsignedListString:noescape()$ har ännu inte
          undertecknat. När alla undertecknat blir dokumentet bindande
          och en kopia skickas till din e-post.
        </p>
        <p>
          Dokumentet har sparats till ditt konto.
        </p>
        $loginModalPart()$
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
        <a class="close cancel">Stäng</a>
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
#####
#####
modalSignedViewNotClosedNoAccount=
<div class="modal-container">
  <form id="accountFromSignForm" method="post" action="$linkaccountfromsign$">
  <div class="modal-header">
    <span class="modal-icon message"></span>
    <span class="modal-title">
      <h3>Du har undertecknat!</h3>
      <p>
        $partyUnsignedListString:noescape()$ har ännu inte undertecknat. När alla undertecknat utfärdar SkrivaPå ett verifikat,
        dokumentet blir bindande och en kopia skickas till din e-post.
      </p>
    </span>
    <a class="modal-close close"> </a>
  </div>
  <div class="modal-body">
    <div class="modal-content">
      $accountFromSignInputs()$
    </div>
  </div>
  <div class="modal-footer">
    $accountFromSignFooter()$
  </div>
</div>
#####
accountFromSignInputs=
<div class="account-from-sign-container">
  <div class="login-container">
    <div class="body">
      <div class="wrapper">
        <p>
          Vill du säkra ditt original? SkrivaPå kan även arkivera ett original för din säkerhet. Det kostar ingenting.
        </p>
        <input type="hidden" name="acceptaccount" value="true" />
        <div style="padding:10px 50px;">
         <div>
          <input type="checkbox" name="tos" id="tos" style="width:20px;margin:0px;"/>
          <label for="tos" style="padding-left:5px;">Jag har läst och accepterat <a target="_blank" href="/allmana-villkor">SkrivaPå Allmänna Villkor</a></label>
         </div>
         <table style="width: 250px; margin-top: 5px"><tbody><tr>
           <td><input id="accountTypePrivate" type="radio" name="accounttype" value="private" /> <label for="accountTypePrivate">Privatperson</label></td>
           <td><input id="accountTypeCompany" type="radio" name="accounttype" value="company" /> <label for="accountTypeCompany">Företag</label></td>
         </tr></tbody></table>
         <div style="padding-top:10px">
          <table>
            <tr>
              <td style="width:120px;">Lösenord</td>
              <td><input type="password" name="password" /></td>
            </tr>
            <tr>
              <td>Bekräfta lösenord</td>
              <td><input type="password" name="password2" /></td>
            </tr>
          </table>
         </div>
        </div>
      </div>
    </div>
  </div>
</div>
#####
accountFromSignFooter=
    <div class="accountFromSignFooter">
      <div class="float-left">
        <a class="declineAccountFromSign cancel" style="width:60px;">Nej tack</a>
      </div>
      <a class="btn-small green float-right submit notForIE7">
        <div class="left"></div>
        <div class="label">
          <p>Ja, arkivera</p>
          <div class="btn-symbol green arrow-left"></div>
        </div>
        <div class="right"></div>
      </a>
    </div>
#####
#####
loginModal=
$if(!logged)$
$if(loginModal)$
<div class="modalcontainer">
  <div class="modalbox overlay login-container">
    <div class="modal-container">
      <form id="loginForm" method="post" action="$linklogin$">
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
<form action="$linkchangepassword$" method="post">
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
        <div class="recovery-container">
          <div class="body">
            <div class="wrapper" style="text-align: right;">
              <span class="txt">Nytt lösenord:</span>
              <input type="password" name="password" autocomplete="off" />
              <BR/> 
              <span class="txt">Upprepa lösenord:</span>
              <input type="password" name="password2" autocomplete="off" />
              <BR/>
              <input type="submit" class="hidden"/>
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
        <a class="close cancel">Avbryt</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
        <div class="left"></div>
        <div class="label">
          <p>Spara</p>
        </div>
        <div class="right"></div>
      </a>
    </div>
  </div>
</form>
####
modalInviteUserAsSubaccount=
<form action="" method="post">
  <input type="hidden" name="takeover" value="really" />
  <input type="hidden" name="email" value="$email$" />
  <input type="hidden" name="fstname" value="$fstname$" />
  <input type="hidden" name="sndname" value="$sndname$" />
  <div class="modal-container">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
	    <h1>Skicka förfrågan?</h1>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">   
      <div class="modal-content"> 
	    <div class="recovery-container">
	      <div class="body">
	        <p>Skicka en förfrågan till användaren att bli ett underkonto
	          under ditt konto. Det betyder att du blir
	          huvudkonto och kommer att ta över ägarskapet över samtliga underkontots dokument. 
	        </p>
	        <p>
	          Är du säker att du vill skicka förfrågan till användaren att bli
	          underkonto?
	        </p>
	      </div>
	    </div>
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
	    <a class="close cancel">Avbryt</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
	    <div class="left"></div>
	    <div class="label">
	      <p>Skicka förfrågan</p>
	    </div>
	    <div class="right"></div>
      </a>
    </div>
  </div>
</form>
####
####
modalDoYouWantToBeSubaccount=
<form action="" method="post">
  <div class="modal-container">
    <div class="modal-header">
      <span class="modal-icon message"></span>
      <span class="modal-title">
	    <h1>Vill du acceptera att bli underkonto?</h1>
      </span>
      <a class="modal-close close"> </a>
    </div>
    <div class="modal-body">   
      <div class="modal-content"> 
	    <div class="recovery-container">
	      <div class="body">
	        <p>Om du accepterar blir du underkonto till huvudkontot
	          $supervisor.fullnamePlusEmail$ och huvudkontot tar över
	          ägarskapet över samtliga dokument i ditt konto.  Är du
	          säker att du vill acceptera?
	        </p>
	      </div>
	    </div>
      </div>
    </div>
    <div class="modal-footer">
      <div class="float-left">
	    <a class="close cancel">Avbryt</a>
      </div>
      <a href="#" class="btn-small blue float-right submit">
	    <div class="left"></div>
	    <div class="label">
	      <p>Acceptera</p>
	    </div>
	    <div class="right"></div>
      </a>
    </div>
  </div>
</form>
####
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

