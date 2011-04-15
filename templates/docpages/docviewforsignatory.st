######
pageDocumentForSignContent=
  <script type="text/javascript" src="/js/bankid.js"></script>
  <div class="localdialogs">
    $loadingdialog()$
    <div style="display: none">
        <script type="text/javascript"> var documentid = "$documentid$", siglinkid = "$siglinkid$", sigmagichash = "$sigmagichash$"; </script>
        <script type="text/javascript">
            $localscripts:noescape()$
        </script>
    </div>
    <form method="post" name="form" action="$action$" id="dialog-confirm-sign" class="overlay">     
    <div class="modal-container">
        <div class="modal-header">
            <span class="modal-icon message"></span>
            <span class="modal-title">
              $if(contract)$ 
                <h2>Underteckna</h2>  
              $elseif(offer)$
                <h2>Godkänn</h2>  
              $endif$
            </span>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content">
              $if(contract)$
                 <p>Är du säker att du vill underteckna dokumentet <strong>$documenttitle$</strong>?</p>
                 <p>När $partyUnsigned:noescape()$ undertecknat blir 
                    avtalet <strong>juridiskt bindande</strong> och
                    det färdigställda avtalet skickas till din
                    e-post.</p>
                 $if(requireseleg)$
                 <span>
                   <h3>Välj typ av e-legitimation</h3>
                   <p>Har du ingen e-legitimation? Du kan ladda ned en
                     e-legitimation från din internetbank eller från
                     Telia. <a href="http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86"
                               target="_new">Klicka här</a> för att läsa mer.</p>
                 </span>
                 $endif$
              $elseif(offer)$
                 <p>Är du säker att du vill godkänna offerten <strong>$documenttitle$</strong>?</p>
              $endif$
            </div>
        </div>
        <div class="modal-footer$if(!requireseleg)$ larger$endif$">
          <input type="hidden" name="sign" value="automatic" id="signauto" />
          <a  class="close cancel float-left">Avbryt</a>
          $if(contract)$
          $if(requireseleg)$
          <span id="elegbuttons">
            <input type="hidden" name="changetoeleg" id="elegprovider" value=""
                   /> 
            <input type="hidden" name="signature" value="" id="signatureinput" />
            <input type="hidden" name="transactionid" value="" id="transactionidinput" />

            <a href="#" class="btn-small green float-right signatory bankid">
              <img src="/img/bankid.png" alt="BankID" />
            </a>

            <a href="#" class="btn-small green float-right signatory telia">
              <img src="/img/telia.png" alt="Telia Eleg" />
            </a>

            <a href="#" class="btn-small green float-right signatory nordea">
              <img src="/img/nordea.png" alt="Nordea Eleg" />
            </a>
          </span>
          $else$
          
          <a href="#" class="btn-big blue float-right submiter">
            <div class="left"></div>
            <div class="label">
              <span>SkrivaPå</span>
              <span class="icon cross"></span>
            </div>
            <div class="right"></div>
          </a>
          $endif$
          $elseif(offer)$
          <a href="#" class="btn-big green float-right submiter">
            <div class="left"></div>
            <div class="label">
              <span>Godkänn offert</span>
            </div>
            <div class="right"></div>
          </a>
         $endif$
        </div>
     </div>

    </form>

    <form method="post" name="form" action="$action$" id="dialog-confirm-cancel" class="overlay">   
    <div class="modal-container">
        <div class="modal-header">
            <span class="modal-icon message"></span>
            <span class="modal-title">
               <h2>Avvisa</h2>   
                <p>Är du säker på att du vill avvisa dokumentet <strong>$documenttitle$</strong>?</p>
                <p>När du avvisat kommer vi att skicka ett e-postmeddelande för att meddela <strong>$authorfstname$ $authorsndname$</strong>.</p>
            </span>
            <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
            <div class="modal-content">
               $rejectMessage:noescape()$
            </div>
        </div>
        <div class="modal-footer">
                <input type="hidden" name="cancel" value="automatic" />
                <div class="float-left">
                  <a  class="close cancel">Avbryt</a>
                  <a  class="editer message">Skriv eget meddelande</a>
                </div>
                <a href="#" class="btn-small red float-right submiter">
                    <div class="left"></div>
                    <div class="label">
                        <p>Avvisa </p>
                    <div class="btn-symbol red arrow-left"></div></div>
                    <div class="right"></div>
                </a>
        </div>
     </div>
    </form> 

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
  <div id="signStepsBody" class="ericfix"> 
    <div id="signViewBodyLeft" > 
      <span id="signViewBodyLeftTextContainer"> 
        $documentinfotext:noescape()$
      </span> 
    </div>
      $signatoriesBox()$
  </div>
</div>

<div id="documentBox">
  <div class="waiting4page"></div>
</div>
$if(notsignedbyme)$$if(pending)$
<div id="signViewBottomBox"> 
  <div id="signViewBottomBoxContainerLeft"> 
    <div id="signViewBottomBoxContainerLeftText"> 
     <div class="float-left"><input type="checkbox" id="signGuardCBox" class="signGuard" autocomplete="off" /></div>
     $if(contract)$
      <div class="float-left" id="signGuardLabel"><label for="signGuardCBox">Jag intygar att jag granskat och förstått dokumentet, att samtliga parter är korrekta och att jag har befogenhet att skriva på.</label></div>
     $elseif(offer)$ 
      <div class="float-left" id="signGuardLabel"><label for="signGuardCBox">Jag intygar att jag granskat och förstått dokumentet, att samtliga parter är korrekta och att jag har befogenhet att godkänt.</label></div>
     $endif$ 
    </div>
  </div>
  <div id="signViewBottomBoxContainerRight"> 
  $if(contract)$
  <a class="btn-big blue center submiter" id="sign" rel="#dialog-confirm-sign" name="sign">
    <div class="left"></div>
    <div class="label" style="width:120px">
      <span>SkrivaPå</span>
      <span class="icon cross"></span>
    </div>
    <div class="right"></div>
  </a>
  <a class="float-right cancel" name="cancel" rel="#dialog-confirm-cancel" id="cancel">Avvisa dokument</a>
  $elseif(offer)$ 
  <a class="btn-big green center submiter" id="sign" rel="#dialog-confirm-sign" name="sign">
    <div class="left"></div>
    <div class="label" style="width:140px;padding-left:20px;padding-right:20px;">
      <span>Godkänn offert</span>
    </div>
    <div class="right"></div>
  </a>
  <a class="float-right cancel" name="cancel" rel="#dialog-confirm-cancel" id="cancel">Avvisa offert</a>
  $endif$
  </div>
</div>
$endif$$endif$ 
$signatoryScriptIfRequired()$
###
####
