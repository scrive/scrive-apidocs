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
       $loginModalPart()$
    </p>
#####
willCreateAccountForYouNoAccount=
    <p>Du kan nu spara dokumentet på SkrivaPå, det kostar ingenting att lagra dokumentet
       och just nu bjuder vi på fri användning av tjänsten i 2 månader!
       Klicka nedan så är det klart.
      <form action="$LinkLandpageSaved()$" method="get">
        <a class="btn-tiny blue submit">
            <div class="left"></div>
            <div class="label"><span>Skapa konto</span></div>
            <div class="right"></div>
        </a>
      </form>
      <BR/>
    </p>
####
loginModal=
$if(!logged)$
$if(loginModal)$
<div class="modalcontainer">
<div class="modalbox overlay">
<div class="modal-container">
      <div class="modal-header">
        <div class="modal-icon message"></div>
        <div class="modal-title">
          <h1>Logga in</h1>
        </div>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content"> 
                $loginModalPart()$
        </div>
      </div>
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
                                                                <form method="post" action="$linklogin$">
                                                                        <input type="hidden" name="referer" value="$referer$" />
                                                                        <input type="email" name="email" infotext="E-post" autocomplete="off"/><br />
                                                                        <input type="password" name="password" infotext="password" autocomplete="off"/><br />
                                                                        <a href="#" class="txt-link">Glömt lösenord</a>
                                                                        <a href="#" class="btn-tiny blue center submit">
                                                                            <div class="left"> </div>     
                                                                            <div class="label"><span>Logga in</span> </div>
                                                                            <div class="right"/> </div>
                                                                        </a>
                                                                        <input type="submit" class="hidden" />
                                                                </form>
                                                        </div>
                                                </div>
                                                <div class="recovery-container">
                                                        <div class="body">
                                                                <form method="post" action="$linkforgotenpassword$">
                                                                        <span class="txt">Skicka nytt lösenord</span>
                                                                        <input type="email" name="email" infotext="E-post" autocomplete="off"/>
                                                                        <a href="#" class="txt-link">Tillbaka</a>
                                                                        <a href="#" class="btn-tiny blue center submit">
                                                                           <div class="left"> </div>     
                                                                            <div class="label"> <span>Skicka</span> </div>
                                                                            <div class="right"/> </div></a>
                                                                        </a>
                                                                        <input type="submit" class="hidden" />
                                                                </form>
                                                        </div>
                                                </div>
</div>
######
####
