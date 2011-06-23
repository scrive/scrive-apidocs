####
pageDocumentForViewerHelpers=
<span class="templating_insert"> 
  <div id="signatory_template" class="signatorybox" alt="$_pageDocumentForViewerHelpersPartyName()$">
  <input name="signatoryname" type="text" value="" autocomplete="off"
         infotext="$_signatoryname()$" /><br/>
  <input name="signatorycompany" type="text" value="" autocomplete="off"
         infotext="$_signatorycompany()$" /><br/>
  <input name="signatorypersonalnumber" type="text" value="" autocomplete="off"
         infotext="$_signatorypersonalnumber()$" /><br/>
  <input name="signatorycompanynumber" type="text" value="" autocomplete="off"
         infotext="$_signatorycompanynumber()$" /><br/>
  <input name="signatoryemail"  type="email" value="" autocomplete="off"
         infotext="$_signatoryemail()$" /><br/>
  <small><a onclick="return signatoryremove(this.parentNode);" href="#">$_signatoryremove()$</a></small>
  </div>
</span>,
<script type="text/javascript"> 
  var documentid = "$documentid$";
  $if(siglinkid)$
  var siglinkid = "$siglinkid$", sigmagichash = "$sigmagichash$";
  $endif$
</script>
###
pageDocumentForViewerContent=
  <div class="localdialogs">
    <div style="display: none">
      $pageDocumentForViewerHelpers()$
    </div>
  </div>
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
         $_pageDocumentForViewerContentDownloadPDF()$
       </a>
     </span>
  </div>
  <div id="signStepsBody" class="forauthor"> 
    <div id="signViewBodyLeft" > 
      <span id="signViewBodyLeftTextContainer"> 
          $documentinfotext:noescape()$
      </span> 

  <div> 
    
  </div>

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

$signatoryScriptIfRequired()$

####
