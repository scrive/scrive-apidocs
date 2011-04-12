####
pageDocumentForAuthor=
  <div class="localdialogs">
    <div style="display: none">
      <script type="text/javascript"> 
        var documentid = "$documentid$", doctokenid = "$doctokenid$", doctokenhash = "$doctokenhash$";
      </script>
    </div>
  </div>
<div id="signStepsContainer"> 
  <div id="signStepsTitleRow"> 
     <span id="signStepsTitleRowTextContainer"> 
      <span class="title">DOKUMENT #$documentid$:</span>
      <span class="name">$documenttitle$</span>
     </span>
     <span class="download">
        <a href="$linkissuedocpdf$" target="_blank">
         Ladda ned PDF
       </a>
     </span>
  </div>
  <div id="signStepsBody" class="forauthor"> 
    <div id="signViewBodyLeft" > 
      <span id="signViewBodyLeftTextContainer"> 
        $documentinfotext:noescape()$
        $if(canberestarted)$
        <BR/><BR/>
        <form method="POST" action="/restart/$documentid$">
          <a class="btn-small red submit">
            <div class="left"></div>
            <div class="label">Starta om dokument</div> 
            <div class="right"></div>
        </a> 
        </form>
        $endif$
        $if(pending)$
        $if(!awaitingauthor)$
        $if(!timetosignset)$
        <a class="btn-small red" id="cancel"  name="cancel" rel="#cancel-by-author-dialog">
            <div class="left"></div>
            <div class="label">Återkalla inbjudan</div> 
            <div class="right"></div>
        </a>
        $endif$
        $endif$
        $endif$
      </span> 
    </div>
     $signatoriesBox()$
  </div>
</div>

<div id="documentBox">
  <script type="text/javascript">
    var docstate = $docstate:noescape()$;
  </script>

  <div class="waiting4page"></div>
</div>

$if(awaitingauthor)$
<script type="text/javascript" src="/js/bankid.js"> </script>
<div style="display:none;">
$validationdropdown()$
</div>
<div id="signViewBottomBox"> 
  <div id="signViewBottomBoxContainerLeft">
    <div id="signViewBottomBoxContainerLeftText"> 
     <div class="float-left"><input type="checkbox" id="signGuardCBox" class="signGuard" autocomplete="off" /></div>
     <div class="float-left" id="signGuardLabel"><label for="signGuardCBox">Jag intygar att jag har granskat och förstått innehållet i dokumentet. Jag intygar även att samtliga parter är korrekta och att jag har befogenhet att skriva på.</label></div>
    </div>
  </div>

  <div id="signViewBottomBoxContainerRight"> 
  <a class="btn-big blue center submiter" value="SkrivaPå" id="signByAuthor" rel="#dialog-confirm-sign-by-author" name="sign">
    <div class="left"></div>
    <div class="label" style="width:120px">
      <span>SkrivaPå</span>
      <span class="icon cross"></span>
    </div>
    <div class="right"></div>
  </a>
  <a class="float-right cancel" name="cancel" id="cancel" rel="#cancel-by-author-dialog"  id="cancel">Återkalla inbjudan</a>
  </div>
</div>
$endif$   
<div class="localdialogs">
 <form method="post" action="$linkcancel$" class="overlay" id="cancel-by-author-dialog">
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2> Återkalla inbjudan </h2>
          <p>Är du säker att du vill återkalla din inbjudan att underteckna dokumentet?
             <br/> När du återkallat inbjudan kommer nedanstående meddelande att skickas till dina motparter.
            
          </p>
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">     
          $cancelMailContent:noescape()$
        </div>
      </div>
      <div class="modal-footer">
        <div class="float-left">
          <a class="close cancel">Avbryt</a>
          <a class="editer message">Skriv eget meddelande</a>
        </div>
        <a class="btn-small red float-right submiter">
          <div class="left"></div>
          <div class="label">
            <p>Återkalla inbjudan</p>
            <div class="btn-symbol red arrow-left"></div>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>
    
  </form>
  <form method="post" name="form" action="$action$" id="dialog-confirm-sign-by-author" class="overlay">     
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>Underteckna</h2>   
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">     
          <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
          <p>När $partyUnsigned:noescape()$ undertecknat blir avtalet <strong>juridiskt bindande</strong> och
            det färdigställda avtalet skickas till din e-post.</p>
          <span class="eleghidden" style="display:none;">
            <h3>Välj typ av e-legitimation</h3>
            <p>Har du ingen e-legitimation? Du kan ladda ned en
              e-legitimation från din internetbank eller från
              Telia. <a href="http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86"
                        target="_new">Klicka här</a> för att läsa mer.</p>
          </span>
        </div>
      </div>
      <div class="modal-footer">
        <input type="hidden" name="sign" value="automatic" id="signauto" />
        <a class="close cancel float-left">Avbryt</a>

        <a href="#" class="btn-big blue float-right submiter signhidden" id="editing-invite-text-finished">
            <div class="left"></div>
            <div class="label">
              <span>SkrivaPå</span>
              <span class="icon cross"></span>
            </div>
            <div class="right"></div>
        </a>

        <span class="eleghidden" style="display: none;">
          <input type="hidden" name="changetoeleg" id="elegprovider" value="" />
          <input type="hidden" name="signature" value="" id="signatureinput" />
          <input type="hidden" name="transactionid" value="" id="transactionidinput" />
          <a href="#" class="btn-small green float-right author2 bankid">
              <img src="/img/bankid.png" alt="BankID" />
          </a>
          <a href="#" class="btn-small green float-right author2 telia">
              <img src="/img/telia.png" alt="Telia Eleg" />
          </a>
          <a href="#" class="btn-small green float-right author2 nordea">
              <img src="/img/nordea.png" alt="Nordea Eleg" />
          </a>
        </span>

      </div>
    </div>
  </form>
</div>
$signatoryScriptIfRequired()$
####
