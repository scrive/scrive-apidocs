####
showAccount =
<div class='blocking-box'></div>
<script>
console.log(jQuery(".mainContainer").size())
var account = new Account({companyAdmin : $if(companyAdmin)$true$else$false$endif$, noCompany : $if(noCompany)$true$else$false$endif$});
console.log(jQuery(".mainContainer").size())
jQuery(".mainContainer").append(account.el());

  jQuery(function() {
    BlockingInfo.show('.blocking-box');
  });
</script>
#####
