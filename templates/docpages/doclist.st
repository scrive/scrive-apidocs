####
pageDocumentsList=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='archive'></div>
<script>
  var fromTemplate = {
    month : $month$,
    year : $year$,
    forCompanyAdmin : $if(isadmin)$true$else$false$endif$,
    hasDataRetentionPolicy: $if(hasdataretentionpolicy)$true$else$false$endif$,
    immediateTrash : $if(immediatetrash)$true$else$false$endif$
  };
</script>
  <script src="$cdnbaseurl$/compiled/all/archive-$versioncode$.js" crossorigin="anonymous"></script>
</div>
</div>
####
