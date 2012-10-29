####
showAccount =
<script>
var account = new Account({companyAdmin : $if(companyAdmin)$true$else$false$endif$, noCompany : $if(noCompany)$true$else$false$endif$});
jQuery(".mainContainer").append(account.el());
</script>
#####
