###
contractrestartbuttontext=Starta om dokument
###
offerrestartbuttontext=Starta om offert
###
orderrestartbuttontext=Starta om order
###
contractcancelbuttontext=Återkalla inbjudan
###
offercancelbuttontext=Återkalla offert
###
ordercancelbuttontext=Återkalla order
###
contractcancelbyauthormodaltitle=
         <h2>Återkalla inbjudan</h2> 
          <p>Är du säker att du vill återkalla din inbjudan att underteckna dokumentet?
            <br/> När du återkallat inbjudan kommer nedanstående meddelande att skickas till dina motparter.
          </p>
###
offercancelbyauthormodaltitle=
         <h2>Är du säker att du vill återkalla offerten?</h2>
          <p>När du återkallat offerten kommer nedanstående meddelande att skickas till din motpart.</p>
###
ordercancelbyauthormodaltitle=
         <h2>Är du säker att du vill återkalla orderbekräftelsen?</h2>
          <p>När du återkallat orderbekräftelsen kommer nedanstående meddelande att skickas till din motpart.</p>
###
pageDocumentForAuthor=
<div class="localdialogs">
  <div style="display: none">
    <script type="text/javascript"> 
      var documentid = "$documentid$";
    </script>
  </div>
</div>
<div id="tab-viewer">
  <div id="signStepsContainer"> 
    <div id="signStepsTitleRow"> 
      <span id="signStepsTitleRowTextContainer"> 
        <span class="title">
          $process.title$ #$documentid$:
        </span>
        <span class="name">$documenttitle$</span>
      </span>
      <span class="download">
        <a href="$linkissuedocpdf$" target="_blank">
          Ladda ned PDF
        </a>
      </span>
    </div>
    $if(hasanyattachments)$
    <ul class="tabs">
      <li id="doctab" class="active"><a href="#" id="doctablink">Dokument</a></li>
      <li id="atttab" ><a href="#" id="attachtablink">Bilagor</a></li>
    </ul>
    $endif$
  </div>
  $if(hasanyattachments)$
  <span id="attachmenttabview" style="display:none">
    <div id="signStepsBody" class="ericfix forauthor"> 
      <div id="signViewBodyLeft" >
        <span id="signViewBodyLeftTextContainer"> 
          <div class="attachmentListContainer">  
            <div class="subheader">Bilaga:</div>
            $attachments:{attachment |
            <div><a target="_blank" href="$attachment.linkattachment$">$attachment.attachmentname$</a></div>
            }$

            $if(hassigattachments)$
            <div class="subheader">Bägarda bilagor:</div>
            <table class="authorsigattachment">
              $sigattachments:{attachment |
              <tr><td>
                  $if(attachment.viewerlink)$
                  <a href="$attachment.viewerlink$"
                  target="_blank">$attachment.name$</a>
                  $else$
                  $attachment.name$
                  $endif$
                </td><td>$attachment.signame$</td></tr>
              }$
            </table>
            $endif$

          </div>
        </span>
      </div>
    </div>
    <div id="attachmentbox waiting4page"></div>
  </span>
  $endif$
  <span id="documenttabview">
    <div id="signStepsBody" class="ericfix forauthor"> 
      <div id="signViewBodyLeft" >
        <span id="signViewBodyLeftTextContainer"> 
          $documentinfotext:noescape()$
          $if(canberestarted)$
          <BR/><BR/>
          <form method="POST" action="/restart/$documentid$">
            <a class="btn-small red submit">
              <div class="left"></div>
              <div class="label">
                $process.restartbuttontext$
              </div> 
              <div class="right"></div>
            </a> 
          </form>
          $endif$
          $if(pending)$
          $if(!awaitingauthor)$
          $if(!timetosignset)$
          <a class="btn-small red" id="cancel"  name="cancel" rel="#cancel-by-author-dialog">
            <div class="left"></div>
            <div class="label">
              $process.cancelbuttontext$
            </div> 
            <div class="right"></div>
          </a> 
          $endif$
          $endif$
          $endif$
        </span> 
      </div>
      $signatoriesBox()$
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
        <a class="float-right btn-small red cancel" name="cancel" rel="#cancel-by-author-dialog" id="cancel">
          <div class="left"></div>
          <div class="label">$process.cancelbuttontext$</div>
          <div class="right"></div>
        </a>
      </div>
      <div id="signViewBottomBoxContainerMiddle">
        <div id="signGuardField">
          <div class="float-left"><input type="checkbox" id="signGuardCBox" class="signGuard" autocomplete="off"/></div>
          <div class="float-left" id="signGuardLabel"><label for="signGuardCBox">Jag intygar att:</label>
            <br/>1. Samtliga undertecknande parter är korrekta.
            <br/>2. Att jag granskat och förstått dokumentet.
            <br/>3. Att jag har befogenhet att skriva på dokumentet.
          </div>
        </div>
      </div>
      <div id="signViewBottomBoxContainerRight"> 
        <a class="btn-big blue center submiter" id="sign" rel="#dialog-confirm-sign-by-author" name="sign">
          <div class="left"></div>
          <div class="label" style="width:120px">
            <span>SkrivaPå</span>
            <span class="icon cross"></span>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>
    $endif$   
  </span>
  <div class="localdialogs">
    $loadingdialog()$
    <form method="post" action="$linkcancel$" class="overlay" id="cancel-by-author-dialog">
      <div class="modal-container">
        <div class="modal-header">
          <span class="modal-icon message"></span>
          <span class="modal-title">
            $process.cancelbyauthormodaltitle:noescape()$ 
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
              <p> $process.cancelbuttontext$ 
              </p>
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

          <a href="#" class="btn-small blue float-right submiter signhidden" id="editing-invite-text-finished">
            <div class="left"></div>
            <div class="label">
              <p>SkrivaPå</p>
              <span class="btn-symbol cross"></span>
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
</div>
$signatoryScriptIfRequired()$
####
