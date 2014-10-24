####
pageDocumentsList=
<div class="mainContainer" >
<div class="body-container">
<div class='blocking-box'></div>
<div class='archive'></div>
<script>
 require(['jquery', 'legacy_code'], function(jQuery) {
  jQuery(".archive").append(new Archive({
    month : $month$, 
    year : $year$, 
    forCompanyAdmin : $if(isadmin)$true$else$false$endif$,
    idledoctimeout : $if(idledoctimeout)$$idledoctimeout$$else$null$endif$,
    idleRemark : "$_documentsArchiveIdleRemark()$",
    idleRemark1 : "$_documentsArchiveIdleRemark1()$",
  }).view().el);

  mixpanel.register({Context : 'Archive'});
  mixpanel.track('View Archive');
 });
</script>
</div>
</div>
####
