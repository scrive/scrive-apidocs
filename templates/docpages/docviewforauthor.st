###
contractrestartbuttontext=$_contractrestartbuttontext()$
###
offerrestartbuttontext=$_offerrestartbuttontext()$
###
orderrestartbuttontext=$_orderrestartbuttontext()$
###
contractcancelbuttontext=$_contractcancelbuttontext()$
###
offercancelbuttontext=$_offercancelbuttontext()$
###
ordercancelbuttontext=$_ordercancelbuttontext()$
###
contractcancelbyauthormodaltitle=
         <h2>$_contractcancelbyauthormodaltitleHead()$</h2>
          <p>$_contractcancelbyauthormodaltitleBody()$</p>
###
offercancelbyauthormodaltitle=
         <h2>$_offercancelbyauthormodaltitleHead()$</h2>
          <p>$_offercancelbyauthormodaltitleBody()$</p>
###
ordercancelbyauthormodaltitle=
         <h2>$_ordercancelbyauthormodaltitleHead()$</h2>
          <p>$_ordercancelbyauthormodaltitleBody()$</p>
###
pageDocumentForAuthor=

<!--
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
          $_DownloadPDF()$
        </a>
      </span>
    </div>
    $if(hasanyattachments)$
    <ul class="tabs">
      <li id="doctab" class="active"><a href="#" id="doctablink">$_Document()$</a></li>
      <li id="atttab" ><a href="#" id="attachtablink">$_Attachments()$</a></li>
    </ul>
    $endif$
  </div>
  $if(hasanyattachments)$
  <span id="attachmenttabview" style="display:none">
    <div id="signStepsBody" class="ericfix forauthor"> 
       <div class="attachmentListContainer">
            <div class="float-left">
             <div class="subheader">$_Attachment()$</div>
             $attachments:{attachment |
              <div><a target="_blank" href="$attachment.linkattachment$">$attachment.attachmentname$</a></div>
             }$
            </div>
            $if(hassigattachments)$
            <div class="float-left" style="margin-left:20px;">
            <div class="subheader">$_RequestedAttachments()$</div>
            <table class="authorsigattachment">
              $sigattachments:{attachment |
              <tr><td>
                  $if(attachment.viewerlink)$
                  <a href="$attachment.viewerlink$"
                  style="margin-right:10px"
                  target="_blank">$attachment.name$</a>
                  $else$
                  <span  style="margin-right:10px">$attachment.name$</span>
                  $endif$
                </td><td>$attachment.signame$</td></tr>
              }$
            </table>
            </div>
            $endif$

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
  </span>
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
        <div class="float-left" id="signGuardLabel"><label for="signGuardCBox">$_signGuardField1()$</label>$_signGuardField2()$

        </div>
      </div>
    </div>
    <div id="signViewBottomBoxContainerRight"> 
      <a class="btn-big blue center submiter" id="sign" rel="#dialog-confirm-sign-by-author" name="sign">
        <div class="left"></div>
        <div class="label" style="width:120px">
          <span>$_skrivapa()$</span>
          <span class="icon cross"></span>
        </div>
        <div class="right"></div>
      </a>
    </div>
  </div>
  $endif$   
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
            <a class="close cancel">$_cancel()$</a>
            <a class="editer message">$_cancelByAuthorDialogOwnMessage()$</a>
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
            <h2>$_dialogConfirmSignByAuthorHead()$</h2>
          </span>
          <a class="modal-close close"> </a>
        </div>
        <div class="modal-body">
          <div class="modal-content">     
            <p>$_dialogConfirmSignByAuthorBody1()$<strong>$documenttitle$</strong>?</p>
            <p>$_dialogConfirmSignByAuthorBody2()$ $partyUnsigned:noescape()$ $_dialogConfirmSignByAuthorBody3()$</p>
            <span class="eleghidden" style="display:none;">
              <h3>$_dialogConfirmSignByAuthorElegSubHead()$</h3>
              <p>$_dialogConfirmSignByAuthorElegBody()$<a href="http://www.e-legitimation.se/Elegitimation/Templates/LogolistPageTypeB.aspx?id=86"
                          target="_new">$_dialogConfirmSignByAuthorElegClickHere1()$</a>$_dialogConfirmSignByAuthorElegClickHere2()$ </p>
            </span>
          </div>
        </div>
        <div class="modal-footer">
          <input type="hidden" name="sign" value="automatic" id="signauto" />
          <a class="close cancel float-left">$_cancel()$</a>

          <a href="#" class="btn-small blue float-right submiter signhidden" id="editing-invite-text-finished">
            <div class="left"></div>
            <div class="label">
              <p>$_skrivapa()$</p>
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
-->
<script>

jQuery(function(){
var docview = KontraStandardDocument.init({
    id:"$documentid$"
     $if(siglinkid)$
    , viewer : new DocumentViewer({
        signatoryid : "$siglinkid$",
        magichash : "$sigmagichash$"
        })
    $endif$
});
jQuery(".body-container").replaceWith(docview.view.el)
})
</script>

####
