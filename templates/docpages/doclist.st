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
    idledoctimeout : $if(idledoctimeout)$$idledoctimeout$$else$null$endif$
  };
</script>
  <script src="$cdnbaseurl$/compiled/all/archive-$versioncode$.js"></script>
</div>
</div>
####
