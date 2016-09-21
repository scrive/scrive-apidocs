######
pageDocumentView=
<div class="mainContainer authorview" >
<div class="body-container">
<div class='place-for-authorview'>
</div>
<script>
var fromTemplate = {
  documentId:"$documentid$",
  signatoryid : "$siglinkid$",
  authorcompanyadmin : $if(authorcompanyadmin)$true$else$false$endif$
};
</script>
<script src="$cdnbaseurl$/compiled/all/authorview-$versioncode$.js" crossorigin="anonymous"></script>
</div>
</div>
####
