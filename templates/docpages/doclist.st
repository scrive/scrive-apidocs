####
pageDocumentsList=
<div class='blocking-box'></div>
<div class='archive'></div>
<script>
jQuery(".archive").append(new Archive({month : $month$, year : $year$, forCompanyAdmin : $if(isadmin)$true$else$false$endif$ }).view().el);
var archiveBlocking;
jQuery(function() {
  archiveBlocking = Blocking();
  archiveBlocking.show('.blocking-box');
});
</script>
####
