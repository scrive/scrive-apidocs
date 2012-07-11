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
      <!--<li><a href="/oauth/dashboard">API</a></li>-->
      <li class='float-right'/>
  </ul>
  <div class="tab-container">
      <div class="tab-content account">
      </div>
  </div>
</div>
<script>
var company = CompanyBranding.init({ companyid: null });
var tabcontent = jQuery(".tab-content");
tabcontent.append(company.input());
tabcontent.append(jQuery("<div class='clearfix' />"));
</script>
#####