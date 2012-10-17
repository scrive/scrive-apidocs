####
pageDocumentsList=
<div class='blocking-box'></div>
<div class='archive'></div>
<script>
jQuery(".archive").append(new Archive({month : $month$, year : $year$, forCompanyAdmin : $if(isadmin)$true$else$false$endif$ }).view().el);
jQuery(function() {
  Blocking().show('.blocking-box');
});
</script>
####
