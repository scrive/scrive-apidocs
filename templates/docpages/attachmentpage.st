####
pageAttachment=
<div style="display: none">
  <script type="text/javascript"> 
    var documentid = "$documentid$";
    $if(siglinkid)$
      var siglinkid = "$siglinkid$";
    $endif$
    $if(sigmagichash)$
      var sigmagichash = "$sigmagichash$";
    $endif$
  </script>
</div>

<div id="signStepsWrapper">
  <div id="signStepsContainer" class="follow">
    <form action="$renamelink$" method="post">
      <div id="signStepsTitleRow">
        <span id="signStepsTitleRowTextContainer">
          <span class="title">BILAGOR:</span>
          $if(editable)$ 
          <span class="docname">
            <span class="docname-display">
              <a href="#" class="icon edit" style="margin-right: 2px"></a>
              <span class="visible-docname">$documenttitle$</span>
            </span>     
            <span class="docname-edit" style="display:none;">
              <a href="#" class="icon small ok submitafterdone" style="margin-right: 2px; float: none"></a>
              <input type="text" name="docname" value="$documenttitle$" />
            </span>
          </span>
          $else$
          <span class="docname">
            <span class="docname-display">
              <span class="visible-docname">$documenttitle$</span>
            </span>
          </span>
          $endif$
        </span>
        <span class="download">
          <a href="$linkissuedocpdf$" target="_blank">
            Ladda ned PDF
          </a>
        </span>
      </div>
     </form>
  </div>
</div>
<div id="documentBox">
  $if(!siglinkid)$
  <script src="/js/templates.js" type="text/javascript"></script>
  $endif$
  <div class="waiting4page"></div>
</div>


