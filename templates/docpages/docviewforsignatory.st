####
contractrejectbuttontext=Avvisa dokument
####
offerrejectbuttontext=Avvisa offert
####
orderrejectbuttontext=Avvisa order
####
contractsignatorysignmodaltitle=Är du säker att du vill underteckna?
####
offersignatorysignmodaltitle=Är du säker att du vill acceptera offerten?
####
ordersignatorysignmodaltitle=Är du säker att du vill bekräfta ordern?
####
contractsignatorysignmodalcontent=
          <p>När $partyUnsigned:noescape()$ undertecknat blir 
            avtalet <strong>juridiskt bindande</strong> och
            det färdigställda avtalet skickas till din
            e-post.</p>
####
offersignatorysignmodalcontent=
          <p> Genom att klicka på "acceptera offert" kommer du att acceptera offerten <strong>$documenttitle$</strong>. När du accepterat offerten skickas en bekräftelse till din och avsändarens inkorg.</p>
####
ordersignatorysignmodalcontent=
          <p>Genom att klicka ”bekräfta order” kommer du att bekräfta ordern <strong>$documenttitle$</strong>. När du har bekräftat order skickas ett slutgiltigt dokument till din och avsändarens inkorg.</p>
####
contractsignbuttontext=SkrivaPå
####
offersignbuttontext=Acceptera offert
####
ordersignbuttontext=Bekräfta order
####
contractsignatorycancelmodaltitle=
          <h2>Avvisa</h2>   
          <p>När du avvisat kommer vi att skicka ett e-postmeddelande för att meddela <strong>$authorfstname$ $authorsndname$</strong>.</p>
####
offersignatorycancelmodaltitle=
          <h2>Är du säker på att du vill avvisa?</h2>   
          <p> När du avvisat kommer vi att skicka ett e-postmeddelande för att meddela <strong>$authorfstname$ $authorsndname$</strong>. 
            Du kan granska meddelandet nedan och justera innehållet genom att klicka på "skriv eget meddelande".
          </p>
####
ordersignatorycancelmodaltitle=
          <h2>Är du säker på att du vill avvisa?</h2>   
          <p> När du avvisat kommer vi att skicka ett e-postmeddelande för att meddela <strong>$authorfstname$ $authorsndname$</strong>. 
            Du kan granska meddelandet nedan och justera innehållet genom att klicka på "skriv eget meddelande".
          </p>
####
contractsignguardwarntext=För att underteckna måste du först klicka i kryssrutan
####
offersignguardwarntext=För att acceptera måste du först klicka i kryssrutan
######
ordersignguardwarntext=För att bekräfta måste du först klicka i kryssrutan
######
pageDocumentForSignContent=
<script type="text/javascript" src="/js/bankid.js"></script>
<div class="localdialogs">
  $loadingdialog()$
  <div style="display: none">
    <script type="text/javascript"> var documentid = "$documentid$", siglinkid = "$siglinkid$", sigmagichash = "$sigmagichash$"; </script>
    <script type="text/javascript">
      $localscripts:noescape()$
      var signguardwarntext = '$process.signguardwarntext$';
    </script>
  </div>

  <form method="post" name="form" action="$action$" id="dialog-confirm-sign" class="overlay">     
    <div class="modal-container">
      <div class="modal-header">
        <span class="modal-icon message"></span>
        <span class="modal-title">
          <h2>$process.signatorysignmodaltitle$</h2>  
        </span>
        <a class="modal-close close"> </a>
      </div>
      <div class="modal-body">
        <div class="modal-content">
          $process.signatorysignmodalcontent:noescape()$
          $if(requireseleg)$
          <span>
            <h3>Välj typ av e-legitimation</h3>
            <p>Har du ingen e-legitimation? Du kan ladda ned en
              e-legitimation från din internetbank eller från
              Telia. <a href="http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86"
                        target="_new">Klicka här</a> för att läsa mer.</p>
          </span>
          $endif$
        </div>
      </div>
      <div class="modal-footer">
        <input type="hidden" name="sign" value="automatic" id="signauto" />
        <a  class="close cancel float-left">Avbryt</a>
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
        
        <a href="#" class="btn-small blue float-right submiter">
          <div class="left"></div>
          <div class="label">
            <p>$process.signbuttontext$</p>
            <span class="btn-symbol cross"></span>
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
          $process.signatorycancelmodaltitle:noescape()$
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
            <p>Avvisa</p>
            <div class="btn-symbol red arrow-left"></div></div>
          <div class="right"></div>
        </a>
      </div>
    </div>
  </form>

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
    $if(signedbyme)$
    $if(hasanyattachments)$
    <ul class="tabs">
      <li id="doctab" class="active"><a href="#" id="doctablink">Dokument</a></li>
      <li id="atttab" ><a href="#" id="attachtablink">Bilagor</a></li>
    </ul>
    $endif$
    $endif$
    
  </div>
  $if(hasanyattachments)$
  <span id="attachmenttabview" style="display:none">
    <div id="signStepsBody" class="ericfix forauthor"> 
      <div id="signViewBodyLeft" >
        <span id="signViewBodyLeftTextContainer"> 
          <div class="attachmentListContainer">  
            $if(isattachments)$
            <div class="subheader">Bifogade bilagor:</div>
            $attachments:{attachment |
            <div><a target="_blank" href="$attachment.linkattachment$">$attachment.attachmentname$</a></div>
            }$
            $endif$
            $if(hassigattachments)$
            <div class="subheader">Bägarda bilagor:</div>
            <table id="signatorysignatoryattachments">
              <tbody>
                $sigattachments:{attachment |
                <tr><td><a href="$attachment.viewerlink$" target="_blank">$attachment.name$</a></td><td>$attachment.signame$</td></tr>
                }$
                </tbody>
            </table>
            $endif$
            
          </div>
        </span>
      </div>
    </div>
  </span>
  $endif$
  <span id="documenttabview">

    <div id="signStepsBody" class="ericfix"> 
      <div id="signViewBodyLeft" > 
        <span id="signViewBodyLeftTextContainer"> 
          $documentinfotext:noescape()$
        </span> 
      </div>
      $signatoriesBox()$
    </div>

  </span>
  <div id="documentBox">
    <div class="waiting4page"></div>
  </div>
  

    $if(notsignedbyme)$

    $if(isattachments)$
    <div id="signViewAttachmentBox">  
      <h2>Bilaga</h2>
      <div id="signViewAttachmentBoxList">
        $attachments:{attachment |
        <div><a target="_blank" href="$attachment.linkattachment$">$attachment.attachmentname$</a></div>
        }$
      </div>
    </div>
    $endif$


    $if(notsignedbyme)$
    $if(hasmysigattachments)$
    
    <div id="signViewSigAttachmentBox">  
      <h2>Begärda bilagor</h2>
      <table id="signViewSigAttachmentBoxList">
        <tbody>
          $mysigattachments:{attachment |
          <tr>
            <td>$attachment.name$</td>
            <td>$attachment.desc$</td>
            <td>              
              $if(attachment.filename)$
              <a href="$attachment.viewerlink$"
                 target="_blank">$attachment.filename$</a>
              $else$
              <form method="post" enctype="multipart/form-data">
                <input type="hidden" name="sigattachment" value="yes" />
                <input type="hidden" name="attachname"
                       value="$attachment.name$" />
                  <a class="btn-small green">  
                    <div class="left"> </div>
                    <div class="label" style="width:80px">Välj fil</div> 
                    <div class="right"> </div>                 
                    <input class="multiFileInput submitOnUpload" style="top:-40px;right:0px" maxlength="20" type="file" name="sigattach" accept="application/pdf" rel="#tobeattached-$attachment.name$" />
                  </a>
                <div id="tobeattached-$attachment.name$" style="display:none;"></div>
              </form>


              $endif$
            </td>
          </tr>
          }$
        </tbody>
      </table>
    </div>
    $endif$

    <div id="signViewBottomBox">
      <div id="signViewBottomBoxContainerLeft">
        <a class="float-right btn-small red cancel" name="cancel" rel="#dialog-confirm-cancel" id="cancel">
          <div class="left"></div>
          <div class="label">$process.rejectbuttontext$</div>
          <div class="right"></div>
        </a>
      </div>
      $if(process.requiressignguard)$
      <div id="signViewBottomBoxContainerMiddle">
        <div id="signGuardField">
          <div class="float-left">
            <input type="checkbox" id="signGuardCBox"
            class="signGuard" autocomplete="off"/>
          </div>
          <div class="float-left" id="signGuardLabel">
            <label for="signGuardCBox">Jag intygar att:
              <br/>1. Samtliga undertecknande parter är korrekta.
              <br/>2. Att jag granskat och förstått dokumentet.
              <br/>3. Att jag har befogenhet att skriva på dokumentet.
            </label>
          </div>
        </div>
      </div>
      $else$
      <div id="signViewBottomBoxContainerMiddle"> 
        <input type="checkbox" id="signGuardCBox" class="signGuard" autocomplete="off" style="display:none" checked="checked" />
      </div>
      $endif$
      <div id="signViewBottomBoxContainerRight"> 
        <a class="btn-big blue center submiter" id="sign" rel="#dialog-confirm-sign" name="sign">
          <div class="left"></div>
          <div class="label" style="$if(process.requiressignguard)$width:120px$else$padding-left:20px;padding-right:20px;$endif$">
            <span>$process.signbuttontext$</span>
            <span class="icon cross"></span>
          </div>
          <div class="right"></div>
        </a>
      </div>
    </div>
    $endif$
    $endif$ 
</div>
$signatoryScriptIfRequired()$
###
#####
