####
showAccount =
<div class='blocking-box'></div>
<script>
var account = new Account({companyAdmin : $if(companyAdmin)$true$else$false$endif$, noCompany : $if(noCompany)$true$else$false$endif$});
jQuery(".mainContainer").append(account.el());
  });
  var accountBlocking = null;
  jQuery(function() {
    accountBlocking = Blocking();
    accountBlocking.show('.blocking-box');
</script>
#####
