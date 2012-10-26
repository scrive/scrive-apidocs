####
viewCompany =
<div class="tab-viewer">
  <div class="tab-header">
  </div>
  <ul class="tabs">
      <li class='float-left'><a href="/account">$_HeadUserInfo()$</a></li>
      <li class='float-left'><a href="/account/security">$_HeadAccountSettings()$</a></li>
      <li class="active float-left"><a href="/account/company">$_CompanySettings()$</a></li>
      <li class='float-left'><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>
      <li class='float-left'><a href="/account/mailapi">$_HeadMailApi()$</a></li>
      <li class='float-left'><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
      $if(seessubscriptiondashboard)$<li class="float-left"><a href="/payments/dashboard">$_Subscription()$</a></li>$endif$
      <!--<li><a href="/oauth/dashboard">API</a></li>-->
      <li class='float-right'/>
  </ul>
  <div class="place-for-branding">
  </div>
</div>
<script>
var company = new CompanyBranding({ companyid: null });
jQuery('.place-for-branding').append(company.el());
</script>
#####