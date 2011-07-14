####
contractrejectbuttontext=$_contractrejectbuttontext()$
####
offerrejectbuttontext=$_offerrejectbuttontext()$
####
orderrejectbuttontext=$_orderrejectbuttontext()$
####
contractsignatorysignmodaltitle=$_contractsignatorysignmodaltitle()$
####
offersignatorysignmodaltitle=$_offersignatorysignmodaltitle()$
####
ordersignatorysignmodaltitle=$_ordersignatorysignmodaltitle()$
####
contractsignatorysignmodalcontent=
          <p>$_contractsignatorysignmodalcontent1()$$partyUnsigned:noescape()$ $_contractsignatorysignmodalcontent2()$</p>
####
offersignatorysignmodalcontent=
          <p>$_offersignatorysignmodalcontent1()$<strong>$documenttitle$</strong>$_offersignatorysignmodalcontent2()$</p>
####
ordersignatorysignmodalcontent=
          <p>$_ordersignatorysignmodalcontent1()$<strong>$documenttitle$</strong>$_ordersignatorysignmodalcontent2()$</p>
####
contractsignbuttontext=$_contractsignbuttontext()$
####
offersignbuttontext=$_offersignbuttontext()$
####
ordersignbuttontext=$_ordersignbuttontext()$
####
contractsignatorycancelmodaltitle=
          <h2>$_contractsignatorycancelmodaltitleHead()$</h2>
          <p>$_contractsignatorycancelmodaltitleBody()$<strong>$authorfstname$ $authorsndname$</strong>.</p>
####
offersignatorycancelmodaltitle=
          <h2>$_offersignatorycancelmodaltitleHead()$</h2>
          <p>$_offersignatorycancelmodaltitleBody()$<strong>$authorfstname$ $authorsndname$</strong>$_offersignatorycancelmodaltitleBody2()$</p>
####
ordersignatorycancelmodaltitle=
          <h2>$_ordersignatorycancelmodaltitleHead()$</h2>
          <p>$_ordersignatorycancelmodaltitleBody()$<strong>$authorfstname$ $authorsndname$</strong>$_ordersignatorycancelmodaltitleBody2()$</p>
####
contractsignguardwarntext=$_contractsignguardwarntext()$
####
offersignguardwarntext=$_offersignguardwarntext()$
######
ordersignguardwarntext=$_ordersignguardwarntext()$
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
            <h3>$_pageDocumentForSignContentElegSubhead()$</h3>
            <p>$_pageDocumentForSignContentElegBody1()$<a href="http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86"
                        target="_new">$_pageDocumentForSignContentElegBody2ClickHere()$</a>$_pageDocumentForSignContentElegBody3()$</p>
          </span>
          $endif$
        </div>
      </div>
      <div class="modal-footer">
        <input type="hidden" name="sign" value="automatic" id="signauto" />
        <a  class="close cancel float-left">$_cancel()$</a>
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
          <a  class="close cancel">$_cancel()$</a>
          <a  class="editer message">$_pageDocumentForSignContentRejectFooterOwnMessage()$</a>
        </div>
        <a href="#" class="btn-small red float-right submiter">
          <div class="left"></div>
          <div class="label">
            <p>$_pageDocumentForSignContentRejectFooterReject()$</p>
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
          $_signStepsTitleRowTextContainerDownloadPDF()$
        </a>
      </span>
    </div>
    $if(signedbyme)$
    $if(hasanyattachments)$
    <ul class="tabs">
      <li id="doctab" class="active"><a href="#" id="doctablink">$_signStepsContainerSignedbymeHasattachmentDoc()$</a></li>
      <li id="atttab" ><a href="#" id="attachtablink">$_signStepsContainerSignedbymeHasattachmentAttachments()$</a></li>
    </ul>
    $endif$
    $endif$
    
  </div>
  $if(hasanyattachments)$
  <span id="attachmenttabview" style="display:none">
    <div id="signStepsBody" class="ericfix forauthor"> 
       <div class="attachmentListContainer">
            $if(isattachments)$
            <div class="float-left" >
            <div class="subheader">$_signStepsBodyAttachmentsSubhead()$</div>
            $attachments:{attachment |
            <div><a target="_blank" href="$attachment.linkattachment$">$attachment.attachmentname$</a></div>
            }$
            </div>
            $endif$
            $if(hassigattachments)$
            <div class="float-left" style="margin-left:20px;">
            <div class="subheader">$_signStepsBodyRequestedAttachmentsSubhead()$</div>
            <table id="signatorysignatoryattachments">
              <tbody>
                $sigattachments:{attachment |
                <tr><td><a style="margin-right:10px" href="$attachment.viewerlink$" target="_blank">$attachment.name$</a></td><td>$attachment.signame$</td></tr>
                }$
                </tbody>
            </table>
            </div>
            $endif$
            
          </div>
    </div>
    <div id="attachmentbox waiting4page"></div>
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
      <h2>$_signViewNotsignedbymeAttachmentBoxHead()$</h2>
      <div id="signViewAttachmentBoxList">
        $attachments:{attachment |
        <div><a target="_blank" href="$attachment.linkattachment$">$attachment.attachmentname$</a></div>
        }$
      </div>
    </div>
    $endif$
    
    $endif$

    $if(notsignedbyme)$
    $if(hasmysigattachments)$
    
    <div id="signViewSigAttachmentBox">  
      <h2>$_signViewSigAttachmentBoxHead()$</h2>
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
                    <div class="label" style="width:80px">$_signViewSigAttachmentBoxChooseFile()$</div>
                    <div class="right"> </div>                 
                    <input class="multiFileInput submitOnUpload" maxlength="20" type="file" name="sigattach" accept="application/pdf" rel="#tobeattached-$attachment.name$" />
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

    $if(pending)$
    
    <div id="signViewBottomBox">
      <div id="signViewBottomBoxContainerLeft">
        <a class="btn-small red cancel" name="cancel" rel="#dialog-confirm-cancel" id="cancel">
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
            <label for="signGuardCBox">$_signGuardCBox()$
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
