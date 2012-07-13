##
subscriptionDashboard =
<script>
  bootPaymentsDashboard('.js-paymentsdashboard');
</script>
<div class="tab-viewer paymentsdashboard">
  <div class="tab-header">
  </div>
  <ul class="tabs">
    <li><a href="/account">$_HeadUserInfo()$</a></li>
    <li><a href="/account/security">$_HeadAccountSettings()$</a></li>
    $if(iscompanyadmin)$<li><a href="/account/company">$_CompanySettings()$</a></li>$endif$
    $if(iscompanyadmin)$<li><a href="/account/companyaccounts">$_CompanyAccounts()$</a></li>$endif$
    <li><a href="/account/mailapi">$_HeadMailApi()$</a></li>
    <li><a href="/account/usagestats">$_HeadUsageStats()$</a></li>
    <li><a href="/oauth/dashboard">API</a></li>
    <li class="active"><a href="/payments/dashboard">$_Subscription()$</a></li>
  </ul>
  <div class="tab-container">
    <div class="tab-content account js-paymentsdashboard payments-dashboard">
    </div>
  </div>
</div>
##