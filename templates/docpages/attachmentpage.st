####
pageAttachment=
<div style="display: none">
  <script type="text/javascript"> 
    var documentid = "$id$";
  </script>
</div>

<div id="signStepsWrapper">
  <div id="signStepsContainer" class="follow">
    <form action="$renamelink$" method="post">
      <div id="signStepsTitleRow">
        <span id="signStepsTitleRowTextContainer">
          <span class="title">$_pageAttachmentAttachments()$</span>
          $if(editable)$ 
          <span class="docname">
            <span class="docname-display">
              <a href="#" class="icon edit" style="margin-right: 2px"></a>
              <span class="visible-docname">$title$</span>
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
            $_pageAttachmentDownloadPDF()$
          </a>
        </span>
      </div>
     </form>
  </div>
</div>
<div id="documentBox">
  <div class="waiting4page"></div>
</div>

