####
pageDocumentForViewerHelpers=
<span class="templating_insert"> 
  <div id="signatory_template" class="signatorybox" alt="Namn på avtalspart">
  <input name="signatoryname" type="text" value="" autocomplete="off"
         infotext="Fullständigt namn" /><br/>
  <input name="signatorycompany" type="text" value="" autocomplete="off"
         infotext="Företag" /><br/>
  <input name="signatorypersonalnumber" type="text" value="" autocomplete="off"
         infotext="Pers.nr" /><br/>
  <input name="signatorycompanynumber" type="text" value="" autocomplete="off"
         infotext="Org.nr" /><br/>
  <input name="signatoryemail"  type="email" value="" autocomplete="off"
         infotext="E-post" /><br/>
  <small><a onclick="return signatoryremove(this.parentNode);" href="#">Ta bort</a></small>
  </div>
</span>,
<script type="text/javascript"> 
  var documentid = "$documentid$";
</script>
###
pageDocumentForViewerContent = 
  <div class="localdialogs">
    <div style="display: none">
      $pageDocumentForViewerHelpers()$
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