####
showAccount =
<div class='blocking-box'></div>
<script>
var account = new Account({companyAdmin : $if(companyAdmin)$true$else$false$endif$, noCompany : $if(noCompany)$true$else$false$endif$});
jQuery(".mainContainer").append(account.el());

  var BlockingInfo = Blocking();
  jQuery(function() {
    BlockingInfo.show('.blocking-box');
  });
</script>
#####
